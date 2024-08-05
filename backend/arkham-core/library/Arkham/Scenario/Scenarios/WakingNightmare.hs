module Arkham.Scenario.Scenarios.WakingNightmare (WakingNightmare (..), wakingNightmare) where

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDreamEaters.ChaosBag
import Arkham.Campaigns.TheDreamEaters.Helpers
import Arkham.Campaigns.TheDreamEaters.Meta
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.WakingNightmare.FlavorText
import Arkham.Scenarios.WakingNightmare.Helpers
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Staff))
import Arkham.Treachery.Cards qualified as Treacheries

newtype WakingNightmare = WakingNightmare ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

wakingNightmare :: Difficulty -> WakingNightmare
wakingNightmare difficulty =
  scenario
    WakingNightmare
    "06063"
    "Waking Nightmare"
    difficulty
    [ ".             recordsOffice .                         ."
    , ".             waitingRoom   .                         ."
    , "emergencyRoom .             experimentalTherapiesWard ."
    , ".             basementDoor1 stairwell                 basementDoor3"
    , ".             .             basementDoor2             ."
    ]

instance HasChaosTokenValue WakingNightmare where
  getChaosTokenValue iid tokenFace (WakingNightmare attrs) = case tokenFace of
    Skull -> do
      isEngaged <- selectAny $ EnemyWithTrait Staff <> enemyEngagedWith iid
      pure
        $ if isEngaged
          then toChaosTokenValue attrs Skull 1 3
          else toChaosTokenValue attrs Skull 3 5
    Cultist -> pure $ ChaosTokenValue Cultist NoModifier
    ElderThing -> do
      n <- selectCount InfestedLocation
      pure $ toChaosTokenValue attrs ElderThing n (n + 1)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage WakingNightmare where
  runMessage msg s@(WakingNightmare attrs) = case msg of
    StandaloneSetup -> do
      push $ SetChaosTokens (initChaosBag TheWebOfDreams $ scenarioDifficulty attrs)
      pure s
    PreScenarioSetup -> do
      players <- allPlayers
      lead <- getLeadPlayer
      push
        $ storyWithChooseOne
          lead
          players
          intro1
          [ Label
              "Convince Doctor Maheswaran to come with you while you investigate, for her safety and yours."
              [story players intro2, Record DrMaheswaranJoinedTheInvestigation]
          , Label
              "Convince Doctor Maheswaran to stay with the patients and keep them safe while you investigate."
              [story players intro3, Record DrMaheswaranStayedWithHerPatients]
          ]
      pure s
    Setup -> do
      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.corruptedOrderly, Treacheries.outbreak]
          [ EncounterSet.WakingNightmare
          , EncounterSet.MergingRealities
          , EncounterSet.WhispersOfHypnos
          , EncounterSet.LockedDoors
          , EncounterSet.StrikingFear
          ]

      drMaheswaranInPlay <- getHasRecord DrMaheswaranJoinedTheInvestigation
      drShivaniMaheswaran <- genCard Assets.drShivaniMaheswaran

      (waitingRoom, placeWaitingRoom) <- placeLocationCard Locations.waitingRoom
      otherPlacements <-
        placeLocationCards_
          [Locations.emergencyRoom, Locations.experimentalTherapiesWard, Locations.recordsOffice]
      lead <- getLead

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , placeWaitingRoom
          , MoveAllTo (toSource attrs) waitingRoom
          ]
        <> [ TakeControlOfSetAsideAsset lead drShivaniMaheswaran
           | drMaheswaranInPlay
           ]
        <> otherPlacements

      agendas <-
        genCards [Agendas.hallsOfStMarys, Agendas.theInfestationSpreads, Agendas.hospitalOfHorrors]
      acts <- genCards [Acts.lookingForAnswers, Acts.searchForThePatient, Acts.containingTheOutbreak]
      setAsideCards <-
        mconcat
          <$> sequence
            [ genSetAsideCards
                [ Enemies.corruptedOrderly
                , Treacheries.outbreak
                , Locations.stairwell
                , Locations.morgue
                , Locations.operatingRoom
                , Locations.privateRoom
                ]
            , genCards [Assets.randolphCarterChainedToTheWakingWorld, Stories.theInfestationBegins]
            , pure $ guard (not drMaheswaranInPlay) *> [drShivaniMaheswaran]
            ]

      WakingNightmare
        <$> runMessage
          msg
          ( attrs
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
              & (setAsideCardsL .~ setAsideCards)
          )
    ScenarioResolution r -> do
      players <- allPlayers
      lead <- getLeadPlayer
      investigators <- allInvestigators
      isFullCampaign <- getIsFullCampaign
      case r of
        NoResolution -> do
          anyResigned <- selectAny ResignedInvestigator
          n <- selectCount InfestedLocation
          steps <- getRecordCount StepsOfTheBridge
          if anyResigned
            then do
              pushAll
                [ story players noResolution
                , RecordCount StepsOfTheBridge (steps + n)
                , Record DrMaheswaran'sFateIsUnknown
                , Record RandolphEscapedTheHospitalOnHisOwn
                , addCampaignCardToDeckChoice lead investigators Assets.randolphCarterChainedToTheWakingWorld
                , R5
                ]
            else pushAll [RecordCount StepsOfTheBridge (steps + n), R4]
        Resolution 1 -> do
          pushAll
            $ [ story players resolution1
              , Record DrMaheswaranIsAlive
              ]
            <> [Record TheDreamersGrowWeaker | isFullCampaign]
            <> [ Record RandolphEscapedTheHospitalWithTheInvestigators
               , addCampaignCardToDeckChoice lead investigators Assets.randolphCarterChainedToTheWakingWorld
               , R5
               ]
        Resolution 2 -> do
          pushAll
            $ [ story players resolution2
              , Record DrMaheswaranIsMissing
              ]
            <> [Record TheDreamersGrowWeaker | isFullCampaign]
            <> [ Record RandolphEscapedTheHospitalWithTheInvestigators
               , addCampaignCardToDeckChoice lead investigators Assets.randolphCarterChainedToTheWakingWorld
               , R5
               ]
        Resolution 3 -> do
          pushAll
            [ story players resolution3
            , Record DrMaheswaranIsAlive
            , Record RandolphEscapedTheHospitalWithTheInvestigators
            , addCampaignCardToDeckChoice lead investigators Assets.randolphCarterChainedToTheWakingWorld
            , R5
            ]
        Resolution 4 -> do
          pushAll
            [ story players resolution4
            , Record DrMaheswaranIsMissing
            , Record RandolphEscapedTheHospitalWithTheInvestigators
            , addCampaignCardToDeckChoice lead investigators Assets.randolphCarterChainedToTheWakingWorld
            , R5
            ]
        Resolution 5 -> do
          gainXp <- toGainXp (toSource attrs) getXp
          pushAll $ gainXp <> [EndOfGame Nothing]
        _ -> error "Invalid resolution"
      pure s
    _ -> WakingNightmare <$> runMessage msg attrs
