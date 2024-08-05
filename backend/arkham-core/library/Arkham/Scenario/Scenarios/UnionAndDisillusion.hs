module Arkham.Scenario.Scenarios.UnionAndDisillusion (
  UnionAndDisillusion (..),
  unionAndDisillusion,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.Helpers
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Helpers.SkillTest
import Arkham.Investigator.Cards qualified as Investigators
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message hiding (InvestigatorDamage)
import Arkham.Placement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.UnionAndDisillusion.Helpers
import Arkham.Scenarios.UnionAndDisillusion.Story
import Arkham.Story.Cards qualified as Stories
import Arkham.Trait (Trait (Spectral))
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Zone

newtype UnionAndDisillusion = UnionAndDisillusion ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

unionAndDisillusion :: Difficulty -> UnionAndDisillusion
unionAndDisillusion difficulty =
  scenario
    UnionAndDisillusion
    "05238"
    "Union and Disillusion"
    difficulty
    [ ".              miskatonicRiver ."
    , "unvisitedIsle3 forbiddingShore unvisitedIsle4"
    , "unvisitedIsle1 .               unvisitedIsle2"
    , "unvisitedIsle5 theGeistTrap    unvisitedIsle6"
    ]

instance HasChaosTokenValue UnionAndDisillusion where
  getChaosTokenValue iid chaosTokenFace (UnionAndDisillusion attrs) = case chaosTokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 2 3
    Cultist -> pure $ toChaosTokenValue attrs Cultist 3 4
    Tablet -> pure $ toChaosTokenValue attrs Tablet 3 4
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
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecordedSets =
        mapFromList
          [
            ( MissingPersons
            , [ crossedOut (toCardCode i)
              | i <-
                  [ Investigators.gavriellaMizrah
                  , Investigators.jeromeDavids
                  , Investigators.valentinoRivas
                  , Investigators.pennyWhite
                  ]
              ]
            )
          ]
    , campaignLogRecorded = setFromList [JosefIsAliveAndWell]
    }

instance RunMessage UnionAndDisillusion where
  runMessage msg s@(UnionAndDisillusion attrs) = case msg of
    PreScenarioSetup -> do
      players <- allPlayers
      lead <- getLeadPlayer
      pushAll
        [ story players intro
        , questionLabel
            "This is a point of no return—you will not get the chance to change your mind later. The investigators must decide (choose one):"
            lead
            $ ChooseOne
              [ Label
                  "\"We have to help complete the Lodge’s ritual.\" Completing the ritual should bind the Spectral Watcher and prevent it from doing any more harm."
                  [Record TheInvestigatorsSidedWithTheLodge]
              , Label
                  "\"We have to stop the Lodge’s ritual.\" Disrupting the ritual should release the Spectral Watcher’s tether to the mortal realm."
                  [Record TheInvestigatorsSidedWithTheCoven]
              ]
        ]
      pure s
    StandaloneSetup -> do
      push (SetChaosTokens standaloneChaosTokens)
      pure $ overAttrs (setStandaloneCampaignLog standaloneCampaignLog) s
    Setup -> do
      encounterDeck <-
        buildEncounterDeckExcluding
          [Treacheries.watchersGaze]
          [ EncounterSet.UnionAndDisillusion
          , EncounterSet.InexorableFate
          , EncounterSet.RealmOfDeath
          , EncounterSet.SpectralPredators
          , EncounterSet.AncientEvils
          , EncounterSet.ChillingCold
          ]

      missingPersons <- getRecordedCardCodes MissingPersons

      (unvisitedIsleCards, unplaceUnvisitedIsles) <-
        splitAt 2
          <$> shuffleM
            [ Locations.unvisitedIsleStandingStones
            , Locations.unvisitedIsleMistyClearing
            , Locations.unvisitedIsleForsakenWoods
            , Locations.unvisitedIsleMossCoveredSteps
            , Locations.unvisitedIsleHauntedSpring
            , Locations.unvisitedIsleDecayedWillow
            ]

      setAsideCards <-
        mconcat
          <$> sequence
            [ concatMapM
                (fmap (map toCard) . gatherEncounterSet)
                [EncounterSet.AnettesCoven, EncounterSet.SilverTwilightLodge, EncounterSet.TheWatcher]
            , genCards
                $ [Locations.theGeistTrap, Treacheries.watchersGaze, Enemies.anetteMason, Enemies.josefMeiger]
                <> [Assets.gavriellaMizrah | "05046" `elem` missingPersons]
                <> [Assets.jeromeDavids | "05047" `elem` missingPersons]
                <> [Assets.valentinoRivas | "05048" `elem` missingPersons]
                <> [Assets.pennyWhite | "05049" `elem` missingPersons]
            , genCards unplaceUnvisitedIsles
            ]

      storyCards <-
        genCards
          $ [Stories.gavriellasFate | "05046" `elem` missingPersons]
          <> [Stories.jeromesFate | "05047" `elem` missingPersons]
          <> [Stories.valentinosFate | "05048" `elem` missingPersons]
          <> [Stories.pennysFate | "05049" `elem` missingPersons]

      sidedWithTheLodge <- getHasRecord TheInvestigatorsSidedWithTheLodge
      sidedWithTheCoven <- getHasRecord TheInvestigatorsSidedWithTheCoven
      deceivingTheLodge <- getHasRecord TheInvestigatorsAreDeceivingTheLodge
      inductedIntoTheInnerCircle <- getHasRecord TheInvestigatorsWereInductedIntoTheInnerCircle
      hidTheirKnowledge <- getHasRecord TheInvestigatorsHidTheirKnowledgeOfTheCoven
      keptMementosHidden <- getHasRecord TheInvestigatorsKeptsTheirMementosHidden
      hereticCount <- getRecordCount HereticsWereUnleashedUntoArkham

      (miskatonicRiver, placeMiskatonicRiver) <- placeLocationCard Locations.miskatonicRiver
      (forbiddingShore, placeForbiddingShore) <- placeLocationCard Locations.forbiddingShore
      (unvisitedIsles, placeUnvisitedIsles) <-
        placeLabeledLocationCards "unvisitedIsle" unvisitedIsleCards

      theWatcher <- genCard Enemies.theSpectralWatcher
      placeTheWatcher <- createEnemyWithPlacement_ theWatcher (OutOfPlay SetAsideZone)

      pushAll
        $ [ SetEncounterDeck encounterDeck
          , SetAgendaDeck
          , SetActDeck
          , PlaceDoomOnAgenda hereticCount CanNotAdvance
          , placeMiskatonicRiver
          , MoveAllTo (toSource attrs) miskatonicRiver
          , placeForbiddingShore
          ]
        <> placeUnvisitedIsles
        <> (if sidedWithTheCoven then map lightBrazier (forbiddingShore : unvisitedIsles) else [])
        <> [placeTheWatcher]

      let
        (act3, act4)
          | sidedWithTheLodge = (Acts.beyondTheMistV1, Acts.theBindingRite)
          | sidedWithTheCoven
          , deceivingTheLodge
          , inductedIntoTheInnerCircle =
              (Acts.beyondTheMistV2, Acts.theBrokenRite)
          | sidedWithTheCoven
          , count id [deceivingTheLodge, hidTheirKnowledge, keptMementosHidden] >= 2 =
              (Acts.beyondTheMistV3, Acts.theBrokenRite)
          | otherwise = (Acts.beyondTheMistV4, Acts.theBrokenRite)

      agendas <- genCards [Agendas.theLoversVI, Agendas.crossroadsOfFate]
      acts <- genCards [Acts.theUnvisitedIsle, Acts.fatedSouls, act3, act4]

      UnionAndDisillusion
        <$> runMessage
          msg
          ( attrs
              & ( setAsideCardsL
                    .~ filter
                      (`cardMatch` NotCard (cardIs Enemies.theSpectralWatcher))
                      setAsideCards
                )
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
              & (cardsUnderScenarioReferenceL .~ storyCards)
          )
    ResolveChaosToken _ Skull iid -> do
      mAction <- getSkillTestAction
      when (mAction == Just Circle) $ do
        push $ DrawAnotherChaosToken iid
      pure s
    ResolveChaosToken _ Cultist iid -> do
      damage <- field InvestigatorDamage iid
      horror <- field InvestigatorHorror iid
      when (damage == 0 || horror == 0) $ do
        push
          $ InvestigatorAssignDamage
            iid
            (ChaosTokenEffectSource Cultist)
            DamageAny
            (if damage == 0 then 1 else 0)
            (if horror == 0 then 1 else 0)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> Tablet)) _ _ -> do
      enemies <- select $ EnemyAt (locationWithInvestigator iid) <> EnemyWithTrait Spectral
      player <- getPlayer iid
      unless (null enemies) $ do
        push
          $ chooseOrRunOne
            player
            [ targetLabel enemy [InitiateEnemyAttack $ enemyAttack enemy (ChaosTokenEffectSource Tablet) iid]
            | enemy <- enemies
            ]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget (chaosTokenFace -> ElderThing)) _ _ -> do
      mAction <- getSkillTestAction
      when (mAction == Just Circle) $ runHauntedAbilities iid
      pure s
    ScenarioResolution n -> do
      investigators <- allInvestigatorIds
      players <- allPlayers
      lead <- getLeadPlayer
      case n of
        NoResolution -> pushAll [story players noResolution, R5]
        Resolution 1 -> do
          inductedIntoTheInnerCircle <- getHasRecord TheInvestigatorsWereInductedIntoTheInnerCircle
          deceivingTheLodge <- getHasRecord TheInvestigatorsAreDeceivingTheLodge

          push
            $ storyWithChooseOne lead players resolution1
            $ [Label "Yes" [R2] | inductedIntoTheInnerCircle && not deceivingTheLodge]
            <> [Label "No" [R3]]
        Resolution 2 ->
          pushAll
            [story players resolution2, Record TheTrueWorkOfTheSilverTwilightLodgeHasBegun, GameOver]
        Resolution 3 ->
          pushAll [story players resolution3, Record CarlSanfordPossessesTheSecretsOfTheUniverse, R8]
        Resolution 4 ->
          pushAll [story players resolution4, Record AnetteMasonIsPossessedByEvil, R8]
        Resolution 5 -> do
          -- Right column is easier to check so we use that one
          spellBroken <- getHasRecord TheWitches'SpellWasCast
          josefDisappearedIntoTheMist <- getHasRecord JosefDisappearedIntoTheMist
          hereticsUnleashed <- getRecordCount HereticsWereUnleashedUntoArkham
          let total = count id [spellBroken, josefDisappearedIntoTheMist, hereticsUnleashed >= 2]
          push $ if total >= 2 then R7 else R6
        Resolution 6 ->
          pushAll [story players resolution6, Record CarlSanfordPossessesTheSecretsOfTheUniverse, R8]
        Resolution 7 -> pushAll [story players resolution7, Record AnetteMasonIsPossessedByEvil, R8]
        Resolution 8 -> do
          gainXp <- toGainXp (toSource attrs) getXp
          gavriellaIsAlive <- getHasRecord GavriellaIsAlive
          jeromeIsAlive <- getHasRecord JeromeIsAlive
          pennyIsAlive <- getHasRecord PennyIsAlive
          valentinoIsAlive <- getHasRecord ValentinoIsAlive
          pushAll
            $ [story players resolution8, RemoveCampaignCard Assets.puzzleBox]
            <> [addCampaignCardToDeckChoice lead investigators Assets.gavriellaMizrah | gavriellaIsAlive]
            <> [Record GavriellaIsDead | not gavriellaIsAlive]
            <> [addCampaignCardToDeckChoice lead investigators Assets.jeromeDavids | jeromeIsAlive]
            <> [Record JeromeIsDead | not jeromeIsAlive]
            <> [addCampaignCardToDeckChoice lead investigators Assets.pennyWhite | pennyIsAlive]
            <> [Record PennyIsDead | not pennyIsAlive]
            <> [addCampaignCardToDeckChoice lead investigators Assets.valentinoRivas | valentinoIsAlive]
            <> [Record ValentinoIsDead | not valentinoIsAlive]
            <> gainXp
            <> [EndOfGame Nothing]
        _ -> error "Invalid resolution"
      pure s
    _ -> UnionAndDisillusion <$> runMessage msg attrs
