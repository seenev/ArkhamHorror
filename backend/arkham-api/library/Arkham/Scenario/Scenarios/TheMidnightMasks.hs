module Arkham.Scenario.Scenarios.TheMidnightMasks where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Campaign.Option
import Arkham.CampaignLogKey
import Arkham.Campaigns.NightOfTheZealot.ChaosBag
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types
import Arkham.Helpers.Agenda (getCurrentAgendaStep)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher (
  CardMatcher (..),
  EnemyMatcher (..),
  ExtendedCardMatcher (..),
  basic,
  cardIs,
 )
import Arkham.Message.Lifted hiding (setActDeck, setAgendaDeck)
import Arkham.Resolution
import Arkham.Scenario.Helpers hiding (forceAddCampaignCardToDeckChoice, recordSetInsert)
import Arkham.Scenario.Runner hiding (chooseOrRunOne, createEnemyAt, placeLocationCard, story)
import Arkham.Scenario.Setup
import Arkham.Scenarios.TheMidnightMasks.Story
import Arkham.Token
import Arkham.Trait qualified as Trait
import Control.Lens (non)

newtype TheMidnightMasks = TheMidnightMasks ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theMidnightMasks :: Difficulty -> TheMidnightMasks
theMidnightMasks difficulty =
  scenarioWith
    TheMidnightMasks
    "01120"
    "The Midnight Masks"
    difficulty
    [ "northside downtown easttown"
    , "miskatonicUniversity rivertown graveyard"
    , "stMarysHospital southside yourHouse"
    ]
    (decksL .~ mapFromList [(CultistDeck, [])])

instance HasChaosTokenValue TheMidnightMasks where
  getChaosTokenValue iid chaosTokenFace (TheMidnightMasks attrs) = case chaosTokenFace of
    Skull -> do
      value <- byDifficulty attrs (fieldMax EnemyDoom (EnemyWithTrait Trait.Cultist)) getDoomCount
      pure $ ChaosTokenValue Skull (NegativeModifier value)
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 2)
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

allCultists :: [CardCode]
allCultists =
  map
    toCardCode
    [ Enemies.wolfManDrew
    , Enemies.hermanCollins
    , Enemies.peterWarren
    , Enemies.victoriaDevereux
    , Enemies.ruthTurner
    , Enemies.theMaskedHunter
    ]

instance RunMessage TheMidnightMasks where
  runMessage msg s@(TheMidnightMasks attrs) = runQueueT $ case msg of
    StandaloneSetup -> do
      setChaosTokens (chaosBagContents $ scenarioDifficulty attrs)
      pure s
    PreScenarioSetup -> do
      forcedToFindOthers <- getHasRecord LitaWasForcedToFindOthersToHelpHerCause
      story
        $ introPart1
        $ if forcedToFindOthers then TheMidnightMasksIntroOne else TheMidnightMasksIntroTwo
      story introPart2
      pure s
    Setup -> runScenarioSetup TheMidnightMasks attrs do
      gather EncounterSet.TheMidnightMasks
      gather EncounterSet.ChillingCold
      gather EncounterSet.Nightgaunts
      gather EncounterSet.LockedDoors
      gather EncounterSet.DarkCult

      setAgendaDeck [Agendas.predatorOrPrey, Agendas.timeIsRunningShort]
      setActDeck [Acts.uncoveringTheConspiracy]

      rivertown <- place Locations.rivertown
      southside <- placeOneOf (Locations.southsideHistoricalSociety, Locations.southsideMasBoardingHouse)
      downtown <- placeOneOf (Locations.downtownFirstBankOfArkham, Locations.downtownArkhamAsylum)
      graveyard <- place Locations.graveyard
      placeAll
        [Locations.easttown, Locations.miskatonicUniversity, Locations.northside, Locations.stMarysHospital]

      addExtraDeck CultistDeck =<< shuffle =<< gatherEncounterSet EncounterSet.CultOfUmordhoth

      getHasRecord YourHouseHasBurnedToTheGround >>= \case
        True -> startAt rivertown
        False -> startAt =<< place Locations.yourHouse

      count' <- getPlayerCount
      let acolytes = replicate (count' - 1) Enemies.acolyte
      zipWithM_ enemyAt acolytes [southside, downtown, graveyard]

      whenHasRecord GhoulPriestIsStillAlive $ addToEncounterDeck (Only Enemies.ghoulPriest)
    ResolveChaosToken _ Cultist iid | isEasyStandard attrs -> do
      closestCultists <- select $ NearestEnemyTo iid $ EnemyWithTrait Trait.Cultist
      when (notNull closestCultists) do
        chooseOrRunOne iid
          $ [ targetLabel x [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget x) Doom 1]
            | x <- closestCultists
            ]
      pure s
    ResolveChaosToken _ Cultist iid | isHardExpert attrs -> do
      cultists <- select $ EnemyWithTrait Trait.Cultist
      case cultists of
        [] -> push $ DrawAnotherChaosToken iid
        xs -> pushAll [PlaceTokens (ChaosTokenEffectSource Cultist) (toTarget eid) Doom 1 | eid <- xs]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      push
        $ byDifficulty
          attrs
          (InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Tablet) 1)
          (InvestigatorPlaceAllCluesOnLocation iid (ChaosTokenEffectSource Tablet))
      pure s
    ScenarioResolution NoResolution -> do
      push R1
      pure s
    ScenarioResolution (Resolution n) -> do
      cultistsWeInterrogated <-
        selectMap toCardCode (VictoryDisplayCardMatch $ basic $ CardWithTrait Trait.Cultist <> CardIsUnique)
      agenda <- getCurrentAgendaStep
      inPlayCultistsWhoGotAway <- selectField EnemyCardCode (EnemyWithTrait Trait.Cultist <> UniqueEnemy)
      let
        resolution = if n == 1 then resolution1 else resolution2
        cultistsWhoGotAway =
          inPlayCultistsWhoGotAway
            <> map toCardCode (attrs ^. decksL . at CultistDeck . non [])
            <> [toCardCode Enemies.theMaskedHunter | agenda == 1]
      ghoulPriestDefeated <- selectAny (VictoryDisplayCardMatch $ basic $ cardIs Enemies.ghoulPriest)
      story resolution
      recordSetInsert CultistsWeInterrogated cultistsWeInterrogated
      recordSetInsert CultistsWhoGotAway cultistsWhoGotAway
      when (n == 2) $ record ItIsPastMidnight
      when ghoulPriestDefeated $ crossOut GhoulPriestIsStillAlive
      allGainXp attrs
      endOfScenario
      pure s
    HandleOption option -> do
      whenM getIsStandalone $ do
        case option of
          AddLitaChantler -> do
            investigators <- allInvestigators
            forceAddCampaignCardToDeckChoice investigators Assets.litaChantler
          _ -> error $ "Unhandled option: " <> show option
      pure s
    _ -> TheMidnightMasks <$> liftRunMessage msg attrs
