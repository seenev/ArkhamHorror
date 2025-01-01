module Arkham.Scenario.Scenarios.TheSecretName (TheSecretName (..), theSecretName) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.ClassSymbol
import Arkham.EncounterSet qualified as Set
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Act
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (EnemyDefeated, RevealLocation)
import Arkham.Message.Lifted.Choose
import Arkham.Resolution
import Arkham.Scenario.Deck
import Arkham.Scenario.Helpers hiding (recordSetInsert)
import Arkham.Scenario.Import.Lifted
import Arkham.Scenarios.TheSecretName.Helpers
import Arkham.Scenarios.TheSecretName.Story
import Arkham.Trait (Trait (Extradimensional))
import Arkham.Treachery.Cards qualified as Treacheries

data Metadata = Metadata {brownJenkinDefeated :: Bool, nahabDefeated :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype TheSecretName = TheSecretName (ScenarioAttrs `With` Metadata)
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSecretName :: Difficulty -> TheSecretName
theSecretName difficulty =
  scenario
    (TheSecretName . (`with` Metadata False False))
    "05120"
    "The Secret Name"
    difficulty
    [ ".              .                 .             unknownPlaces4           unknownPlaces1   unknownPlaces2         unknownPlaces3 ."
    , ".              walterGilmansRoom .             cityOfElderThings        physicsClassroom siteOfTheSacrifice     .              strangeGeometry1"
    , "decrepitDoor1  moldyHalls        decrepitDoor2 moldyHallsEarlierTonight keziahsRoom      witchHouseRuins        .              ."
    , ".              decrepitDoor3     .             salemGaol1692            twilightAbyss    courtOfTheGreatOldOnes .              strangeGeometry2"
    , ".              .                 .             .                        unknownPlaces5   unknownPlaces6         unknownPlaces7 ."
    ]

instance HasChaosTokenValue TheSecretName where
  getChaosTokenValue iid chaosTokenFace (TheSecretName (attrs `With` _)) = case chaosTokenFace of
    Skull -> do
      atExtradimensionalLocation <-
        selectAny $ locationWithInvestigator iid <> LocationWithTrait Extradimensional
      pure
        $ if atExtradimensionalLocation
          then toChaosTokenValue attrs Skull 3 4
          else toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 3
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

standaloneChaosTokens :: [ChaosTokenFace]
standaloneChaosTokens =
  [ PlusOne
  , Zero
  , Zero
  , MinusOne
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage TheSecretName where
  runMessage msg s@(TheSecretName (attrs `With` meta)) = runQueueT $ scenarioI18n $ case msg of
    PreScenarioSetup -> do
      anyMystic <- selectAny $ InvestigatorWithClass Mystic
      whenHasRecord TheInvestigatorsAreMembersOfTheLodge do
        storyWithChooseOneM intro1 do
          labeled "Tell the Lodge of the witches in the woods." do
            story intro2
            record TheInvestigatorsToldTheLodgeAboutTheCoven
            addChaosToken Cultist
          labeled "Tell him you know of no possible connection. (You are lying.)" do
            story
              $ intro3
              <> (if anyMystic then intro3Mystic else mempty)
              <> intro3Part2
            record TheInvestigatorsHidTheirKnowledgeOfTheCoven
      whenHasRecord TheInvestigatorsAreEnemiesOfTheLodge $ story intro4
      whenHasRecord TheInvestigatorsLearnedNothingOfTheLodge'sSchemes $ story intro5
      whenHasRecord TheInvestigatorsAreNeverSeenOrHeardFromAgain $ story intro6
      pure s
    StandaloneSetup -> do
      setChaosTokens standaloneChaosTokens
      pure s
    Setup -> runScenarioSetup (TheSecretName . (`with` meta)) attrs do
      gather Set.TheSecretName
      gather Set.CityOfSins
      gather Set.InexorableFate
      gather Set.RealmOfDeath
      gather Set.Witchcraft
      gather Set.Rats

      startAt =<< place Locations.moldyHalls
      place_ Locations.walterGilmansRoom

      placeGroup
        "decrepitDoor"
        [ Locations.landlordsQuarters
        , Locations.joeMazurewiczsRoom
        , Locations.frankElwoodsRoom
        ]

      setAside
        [ Enemies.nahab
        , Locations.siteOfTheSacrifice
        , Locations.keziahsRoom
        , Assets.theBlackBook
        , Locations.strangeGeometry
        , Locations.strangeGeometry
        , Treacheries.ghostlyPresence
        , Treacheries.ghostlyPresence
        ]

      setAgendaDeck
        [ Agendas.theHermitIX
        , Agendas.theFamiliar
        , Agendas.theWitchLight
        , Agendas.markedForSacrifice
        ]
      setActDeck
        [ Acts.investigatingTheWitchHouse
        , Acts.beyondTheWitchHouse
        , Acts.stoppingTheRitual
        ]

      -- Unknown Places Deck
      unknownPlaces <-
        shuffleM
          =<< genCards
            [ Locations.moldyHallsEarlierTonight
            , Locations.twilightAbyss
            , Locations.cityOfElderThings
            , Locations.salemGaol1692
            , Locations.physicsClassroom
            , Locations.courtOfTheGreatOldOnesANotTooDistantFuture
            ]

      let (bottom, top) = splitAt 3 unknownPlaces
      witchHouseRuins <- genCard Locations.witchHouseRuins
      bottom' <- shuffleM $ witchHouseRuins : bottom

      addExtraDeck UnknownPlacesDeck (top <> bottom')
    ResolveChaosToken _ Cultist iid -> do
      push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ ElderThing _ | isHardExpert attrs -> do
      push HuntersMove
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> do
      case token.face of
        Cultist ->
          push
            $ DiscardTopOfEncounterDeck
              iid
              (if isEasyStandard attrs then 3 else 5)
              (toSource attrs)
              Nothing
        Tablet -> do
          selectForMaybeM (enemyIs Enemies.nahab) \nahab -> do
            if isEasyStandard attrs
              then do
                atYourLocation <- nahab <=~> EnemyAt (locationWithInvestigator iid)
                when atYourLocation $ push $ EnemyWillAttack $ enemyAttack nahab attrs iid
              else push $ EnemyWillAttack $ enemyAttack nahab attrs iid
        ElderThing | isEasyStandard attrs -> push HuntersMove
        _ -> pure ()
      pure s
    EnemyDefeated _ cardId _ _ -> do
      isBrownJenkin <- selectAny $ cardIs Enemies.brownJenkin <> CardWithId cardId
      isNahab <- selectAny $ cardIs Enemies.nahab <> CardWithId cardId
      pure
        . TheSecretName
        $ attrs
        `with` meta
          { brownJenkinDefeated = brownJenkinDefeated meta || isBrownJenkin
          , nahabDefeated = nahabDefeated meta || isNahab
          }
    ScenarioResolution resolution -> do
      iids <- allInvestigators
      step <- getCurrentActStep
      lead <- getLead
      let
        brownJenkinBonus = if brownJenkinDefeated meta then toBonus "brownJenkinDefeated" 1 else NoBonus
        nahabBonus = if nahabDefeated meta then toBonus "nahabDefeated" 1 else NoBonus
        addTheBlackBook = chooseOneM lead do
          labeled "Do not add The Black Book" nothing
          targets iids \iid -> do
            addCampaignCardToDeck iid Assets.theBlackBook
            addChaosToken Skull
      case resolution of
        NoResolution -> do
          story noResolution
          push R1
        Resolution 1 -> do
          story resolution1
          allGainXpWithBonus attrs (brownJenkinBonus <> nahabBonus)
          when (step == 2) $ recordSetInsert MementosDiscovered [Gilman'sJournal]
          when (step == 3) $ recordSetInsert MementosDiscovered [Keziah'sFormulae]
          when (step >= 2) addTheBlackBook
          endOfScenario
        Resolution 2 -> do
          story resolution2
          allGainXpWithBonus attrs $ toBonus "resolution2" 2
          recordSetInsert MementosDiscovered [Gilman'sJournal, Keziah'sFormulae, WornCrucifix]
          addTheBlackBook
          endOfScenario
        _ -> error "invalid resolution"
      pure s
    _ -> TheSecretName . (`with` meta) <$> liftRunMessage msg attrs
