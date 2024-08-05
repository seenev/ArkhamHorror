module Arkham.Scenario.Scenarios.BlackStarsRise (
  BlackStarsRise (..),
  blackStarsRise,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Agenda.Types (Field (..))
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.ThePathToCarcosa.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Deck
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Enemy.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.BlackStarsRise.Story
import Arkham.SkillTest
import Arkham.Token
import Arkham.Trait qualified as Trait

newtype BlackStarsRise = BlackStarsRise ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackStarsRise :: Difficulty -> BlackStarsRise
blackStarsRise difficulty =
  scenarioWith
    BlackStarsRise
    "03274"
    "Black Stars Rise"
    difficulty
    [ ".                cloister      .           northTower      ."
    , "knightsHall      cloister      .           northTower      ."
    , "knightsHall      abbeyChurch    brokenSteps .               outerWall"
    , "chapelOfStAubert abbeyChurch    brokenSteps .               outerWall"
    , "chapelOfStAubert chœurGothique  .           grandRue        ."
    , ".                chœurGothique  .           grandRue        ."
    , ".                abbeyTower     .           porteDeLAvancée ."
    , ".                abbeyTower     .           porteDeLAvancée ."
    ]
    (decksLayoutL .~ ["act1 agenda1 agenda2 act2"])

instance HasChaosTokenValue BlackStarsRise where
  getChaosTokenValue iid chaosTokenFace (BlackStarsRise attrs) = case chaosTokenFace of
    Skull -> do
      maxDoom <- fieldMax AgendaDoom AnyAgenda
      totalDoom <- selectSum AgendaDoom AnyAgenda
      pure $ toChaosTokenValue attrs Skull maxDoom totalDoom
    Cultist ->
      if isEasyStandard attrs
        then do
          modifier <- do
            mAction <- getSkillTestAction
            case mAction of
              Just action | action `elem` [Action.Evade, Action.Fight] -> do
                mtarget <- getSkillTestTarget
                case mtarget of
                  Just (EnemyTarget eid) -> do
                    hasDoom <- fieldP EnemyDoom (> 0) eid
                    pure $ if hasDoom then AutoFailModifier else NoModifier
                  _ -> pure NoModifier
              _ -> pure NoModifier
          pure $ ChaosTokenValue Cultist modifier
        else do
          anyEnemyWithDoom <- selectAny EnemyWithAnyDoom
          let
            modifier = if anyEnemyWithDoom then AutoFailModifier else NoModifier
          pure $ ChaosTokenValue Cultist modifier
    Tablet -> pure $ ChaosTokenValue Tablet NoModifier
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 2 3
    otherFace -> getChaosTokenValue iid otherFace attrs

data Version = TheFloodBelow | TheVortexAbove
  deriving stock Eq

versions :: NonEmpty Version
versions = TheFloodBelow :| [TheVortexAbove]

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusThree
  , MinusFour
  , MinusFive
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

instance RunMessage BlackStarsRise where
  runMessage msg s@(BlackStarsRise attrs) = case msg of
    StandaloneSetup -> do
      lead <- getLead
      theManInThePallidMask <- genCard Enemies.theManInThePallidMask
      randomToken <- sample $ Cultist :| [Tablet, ElderThing]
      pushAll
        [ SetChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
        , ShuffleCardsIntoDeck (InvestigatorDeck lead) [theManInThePallidMask]
        ]
      pure s
    Setup -> do
      investigatorIds <- allInvestigatorIds
      players <- allPlayers
      ashleighInterviewed <- interviewed Assets.ashleighClarke
      version <- sample versions

      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.beastOfAldebaran, Enemies.tidalTerror, Enemies.riftSeeker]
          [ EncounterSet.BlackStarsRise
          , EncounterSet.EvilPortents
          , EncounterSet.Byakhee
          , EncounterSet.InhabitantsOfCarcosa
          , EncounterSet.TheStranger
          , EncounterSet.DarkCult
          , EncounterSet.AncientEvils
          ]

      let
        tokenToAdd = case scenarioDifficulty attrs of
          Easy -> MinusThree
          Standard -> MinusFive
          Hard -> MinusSix
          Expert -> MinusSeven
        (agenda2a, agenda2c, abbeyTower, chapelOfStAubert) =
          if version == TheVortexAbove
            then
              ( Agendas.letTheStormRageTheVortexAbove
              , Agendas.theEntityAboveTheVortexAbove
              , Locations.abbeyTowerThePathIsOpen
              , Locations.chapelOfStAubertWatersForbidden
              )
            else
              ( Agendas.letTheStormRageTheFloodBelow
              , Agendas.theEntityAboveTheFloodBelow
              , Locations.abbeyTowerSpiresForbidden
              , Locations.chapelOfStAubertThePathIsOpen
              )

      choeurGothique <-
        sample
          (Locations.choeurGothique_292 :| [Locations.choeurGothique_293])

      setAsideCards <-
        genCards
          [ Enemies.tidalTerror
          , Enemies.tidalTerror
          , Enemies.riftSeeker
          , Enemies.riftSeeker
          , Acts.openThePathAbove
          , Acts.openThePathBelow
          , Enemies.beastOfAldebaran
          , Locations.cloister
          , Locations.knightsHall
          , abbeyTower
          , chapelOfStAubert
          , choeurGothique
          ]

      porteDeLAvancee <- genCard Locations.porteDeLAvancee
      grandRue <- genCard Locations.grandRue
      abbeyChurch <- genCard Locations.abbeyChurch
      northTower <-
        genCard
          =<< sample (Locations.northTower_287 :| [Locations.northTower_288])
      outerWall <-
        genCard
          =<< sample (Locations.outerWall_285 :| [Locations.outerWall_286])
      brokenSteps <-
        genCard
          =<< sample (Locations.brokenSteps_289 :| [Locations.brokenSteps_290])

      isStandalone <- getIsStandalone

      (porteDeLAvanceeId, placePorteDeLAvancee) <- placeLocation porteDeLAvancee
      otherPlacements <-
        traverse
          placeLocation_
          [northTower, outerWall, brokenSteps, grandRue, abbeyChurch]

      pushAll
        $ [story players intro]
        <> [story players ashleighsInformation | ashleighInterviewed]
        <> [ SearchCollectionForRandom
            iid
            (toSource attrs)
            ( BasicWeaknessCard
                <> CardWithOneOf
                  ( map
                      CardWithTrait
                      [Trait.Madness, Trait.Pact, Trait.Cultist, Trait.Detective]
                  )
            )
           | iid <- investigatorIds
           , not isStandalone
           ]
        <> [AddChaosToken tokenToAdd, SetAgendaDeck, SetEncounterDeck encounterDeck]
        <> (placePorteDeLAvancee : otherPlacements)
        <> [MoveAllTo (toSource attrs) porteDeLAvanceeId]

      agendas1 <-
        genCards
          [Agendas.theTideRises, agenda2a, Agendas.theCityFloods]
      agendas2 <-
        genCards
          [ Agendas.theRitualBeginsBlackStarsRise
          , agenda2c
          , Agendas.swallowedSky
          ]

      BlackStarsRise
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (agendaStackL . at 1 ?~ agendas1)
              & (agendaStackL . at 2 ?~ agendas2)
          )
    PlaceDoomOnAgenda n canAdvance -> do
      agendaIds <- select AnyAgenda
      lead <- getLeadPlayer
      pushWhen (canAdvance == CanAdvance) AdvanceAgendaIfThresholdSatisfied
      push
        $ chooseOne
          lead
          [ targetLabel agendaId [PlaceTokens (toSource attrs) (toTarget agendaId) Doom n]
          | agendaId <- agendaIds
          ]
      pure s
    PassedSkillTest _ _ _ (ChaosTokenTarget token) _ n | n < 1 -> do
      when (chaosTokenFace token == Tablet) $ do
        targets <- selectMap AgendaTarget AnyAgenda
        pushAll [PlaceTokens (toSource attrs) target Doom 1 | target <- targets]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      when (chaosTokenFace token == Tablet) $ do
        targets <- selectMap AgendaTarget AnyAgenda
        pushAll [PlaceTokens (toSource attrs) target Doom 1 | target <- targets]
      when (chaosTokenFace token == ElderThing) $ do
        push
          $ findAndDrawEncounterCard iid
          $ CardWithType EnemyType
          <> CardWithTrait Trait.Byakhee
      pure s
    ScenarioResolution res -> do
      ashleighSlain <- selectOne $ VictoryDisplayCardMatch $ cardIs Enemies.ashleighClarke
      gainXp <- toGainXp attrs $ getXp
      iids <- allInvestigatorIds
      players <- allPlayers
      let
        updateSlain =
          [ recordSetInsert VIPsSlain [toCardCode ashleigh]
          | ashleigh <- maybeToList ashleighSlain
          ]
      case res of
        NoResolution -> push $ ScenarioResolution $ Resolution 3
        Resolution 1 -> do
          pushAll
            $ [ story players resolution1
              , Record YouOpenedThePathBelow
              , RemoveAllChaosTokens Cultist
              , RemoveAllChaosTokens Tablet
              , RemoveAllChaosTokens ElderThing
              , AddChaosToken Cultist
              , AddChaosToken Cultist
              , AddChaosToken Tablet
              , AddChaosToken Tablet
              ]
            <> updateSlain
            <> gainXp
            <> [EndOfGame Nothing]
        Resolution 2 -> do
          pushAll
            $ [ story players resolution2
              , Record YouOpenedThePathAbove
              , RemoveAllChaosTokens Cultist
              , RemoveAllChaosTokens Tablet
              , RemoveAllChaosTokens ElderThing
              , AddChaosToken Cultist
              , AddChaosToken Cultist
              , AddChaosToken ElderThing
              , AddChaosToken ElderThing
              ]
            <> updateSlain
            <> gainXp
            <> [EndOfGame Nothing]
        Resolution 3 -> do
          pushAll
            $ story players resolution3
            : Record TheRealmOfCarcosaMergedWithOurOwnAndHasturRulesOverThemBoth
            : map DrivenInsane iids
              <> [GameOver]
        _ -> error "Unknown resolution"
      pure s
    RequestedPlayerCard iid source mcard _ | isSource attrs source -> do
      for_ mcard $ push . AddCardToDeckForCampaign iid
      pure s
    _ -> BlackStarsRise <$> runMessage msg attrs
