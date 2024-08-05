module Arkham.Scenario.Scenarios.TheHouseAlwaysWins (
  TheHouseAlwaysWins (..),
  theHouseAlwaysWins,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheDunwichLegacy.ChaosBag
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Location.Cards qualified as Locations
import Arkham.Message
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.ScenarioLogKey
import Arkham.Scenarios.TheHouseAlwaysWins.Story

newtype TheHouseAlwaysWins = TheHouseAlwaysWins ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theHouseAlwaysWins :: Difficulty -> TheHouseAlwaysWins
theHouseAlwaysWins difficulty =
  scenario
    TheHouseAlwaysWins
    "02062"
    "The House Always Wins"
    difficulty
    [ ".           .                .                  backHallDoorway1 ."
    , ".           .                cloverClubCardroom backHallDoorway1 ."
    , "laBellaLuna cloverClubLounge cloverClubCardroom darkenedHall     backHallDoorway2"
    , "laBellaLuna cloverClubLounge cloverClubBar      darkenedHall     backHallDoorway2"
    , ".           .                cloverClubBar      backHallDoorway3 ."
    , ".           .                .                  backHallDoorway3 ."
    ]

instance HasChaosTokenValue TheHouseAlwaysWins where
  getChaosTokenValue iid chaosTokenFace (TheHouseAlwaysWins attrs) = case chaosTokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ ChaosTokenValue Cultist (NegativeModifier 3)
    Tablet -> pure $ ChaosTokenValue Tablet (NegativeModifier 2)
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheHouseAlwaysWins where
  runMessage msg s@(TheHouseAlwaysWins attrs) = case msg of
    StandaloneSetup -> do
      push $ SetChaosTokens $ chaosBagContents attrs.difficulty
      pure s
    Setup -> do
      players <- allPlayers

      encounterDeck <-
        buildEncounterDeckExcluding
          [Assets.peterClover, Enemies.cloverClubPitBoss]
          [ EncounterSet.TheHouseAlwaysWins
          , EncounterSet.BadLuck
          , EncounterSet.NaomisCrew
          , EncounterSet.Rats
          ]

      cloverClubPitBoss <- genCard Enemies.cloverClubPitBoss
      (laBellaLunaId, placeLaBellaLuna) <-
        placeLocationCard
          Locations.laBellaLuna
      (cloverClubLoungeId, placeCloverClubLounge) <-
        placeLocationCard
          Locations.cloverClubLounge
      placeCloverClubBar <- placeLocationCard_ Locations.cloverClubBar
      placeCloverClubCardroom <- placeLocationCard_ Locations.cloverClubCardroom
      createCloverClubPitBoss <-
        createEnemyAt_
          cloverClubPitBoss
          cloverClubLoungeId
          Nothing

      pushAll
        [ SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , placeLaBellaLuna
        , placeCloverClubLounge
        , placeCloverClubBar
        , placeCloverClubCardroom
        , RevealLocation Nothing laBellaLunaId
        , MoveAllTo (toSource attrs) laBellaLunaId
        , createCloverClubPitBoss
        , story players intro
        ]

      setAsideCards <-
        genCards
          [ Locations.darkenedHall
          , Assets.peterClover
          , Assets.drFrancisMorgan
          , Locations.artGallery
          , Locations.vipArea
          , Locations.backAlley
          ]

      acts <-
        genCards
          [Acts.beginnersLuck, Acts.skinGame, Acts.allIn, Acts.fold]
      agendas <-
        genCards
          [ Agendas.theCloverClub
          , Agendas.undergroundMuscle
          , Agendas.chaosInTheCloverClub
          ]

      TheHouseAlwaysWins
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    ResolveChaosToken _ Tablet iid -> s <$ push (SpendResources iid 3)
    ResolveChaosToken drawnToken Skull iid -> do
      let requiredResources = if isEasyStandard attrs then 2 else 3
      resourceCount <- getSpendableResources iid
      player <- getPlayer iid
      withSkillTest \sid -> do
        pushWhen (resourceCount >= requiredResources)
          $ chooseOne
            player
            [ Label
                ( "Spend "
                    <> tshow requiredResources
                    <> " resources to treat this token as a 0"
                )
                [ SpendResources iid requiredResources
                , CreateChaosTokenValueEffect
                    sid
                    (if isEasyStandard attrs then 2 else 3)
                    (ChaosTokenSource drawnToken)
                    (ChaosTokenTarget drawnToken)
                ]
            , Label "Do not spend resources" []
            ]
      pure s
    PassedSkillTest iid _ _ (ChaosTokenTarget token) _ _ ->
      s <$ case chaosTokenFace token of
        Cultist
          | isEasyStandard attrs ->
              push $ TakeResources iid 3 (ChaosTokenEffectSource Cultist) False
        _ -> pure ()
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ ->
      s <$ case chaosTokenFace token of
        Cultist | isHardExpert attrs -> push $ SpendResources iid 3
        Tablet | isEasyStandard attrs -> push $ SpendResources iid 3
        _ -> pure ()
    ScenarioResolution NoResolution ->
      s <$ push (ScenarioResolution $ Resolution 1)
    ScenarioResolution (Resolution 1) -> do
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players resolution1
          , Record OBannionGangHasABoneToPickWithTheInvestigators
          , Record DrFrancisMorganWasKidnapped
          ]
        <> [AddChaosToken ElderThing | Cheated `member` scenarioLog attrs]
        <> [GainXP iid (toSource attrs) (n + 1) | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 2) -> do
      lead <- getLeadPlayer
      players <- allPlayers
      investigatorIds <- allInvestigatorIds
      xp <- getXp
      pushAll
        $ [ story players resolution2
          , Record OBannionGangHasABoneToPickWithTheInvestigators
          , Record TheInvestigatorsRescuedDrFrancisMorgan
          , chooseOne
              lead
              [ Label
                  "Add Dr. Francis Morgan to a deck"
                  [ chooseOne
                      lead
                      [ targetLabel
                        iid
                        [AddCampaignCardToDeck iid Assets.drFrancisMorgan]
                      | iid <- investigatorIds
                      ]
                  ]
              , Label "Do not add Dr. Francis Morgan to any deck" []
              ]
          ]
        <> [AddChaosToken ElderThing | Cheated `member` scenarioLog attrs]
        <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 3) -> do
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players resolution3
          , Record NaomiHasTheInvestigatorsBacks
          , Record DrFrancisMorganWasKidnapped
          ]
        <> [AddChaosToken ElderThing | Cheated `member` scenarioLog attrs]
        <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 4) -> do
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players resolution4
          , Record OBannionGangHasABoneToPickWithTheInvestigators
          , Record DrFrancisMorganWasKidnapped
          , Record InvestigatorsWereUnconsciousForSeveralHours
          ]
        <> [AddChaosToken ElderThing | Cheated `member` scenarioLog attrs]
        <> [GainXP iid (toSource attrs) (n + 1) | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    _ -> TheHouseAlwaysWins <$> runMessage msg attrs
