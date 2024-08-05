module Arkham.Scenario.Scenarios.TheWitchingHour (
  TheWitchingHour (..),
  theWitchingHour,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Act.Sequence (ActStep (..), actStep)
import Arkham.Act.Types (Field (ActSequence))
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.CampaignLogKey
import Arkham.Campaigns.TheCircleUndone.ChaosBag
import Arkham.Campaigns.TheCircleUndone.Memento
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Query
import Arkham.Helpers.Scenario
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message
import Arkham.Movement
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheWitchingHour.Story
import Data.Map.Monoidal qualified as MonoidalMap
import Data.Map.Strict qualified as Map

newtype TheWitchingHour = TheWitchingHour ScenarioAttrs
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theWitchingHour :: Difficulty -> TheWitchingHour
theWitchingHour difficulty =
  scenario
    TheWitchingHour
    "05050"
    "The Witching Hour"
    difficulty
    [ ".      .     woods1        .      . "
    , ".      .     .             .      . "
    , "woods2 .     witchesCircle .      woods3"
    , ".      .     .             .      ."
    , ".      wood4 .             woods5 ."
    ] -- lost and separated, do we label 4 zones, or do a different placement

instance HasChaosTokenValue TheWitchingHour where
  getChaosTokenValue iid chaosTokenFace (TheWitchingHour attrs) = case chaosTokenFace of
    Skull -> pure $ toChaosTokenValue attrs Skull 1 2
    Tablet -> pure $ toChaosTokenValue attrs Skull 1 2
    ElderThing -> pure $ toChaosTokenValue attrs Skull 3 4
    otherFace -> getChaosTokenValue iid otherFace attrs

instance RunMessage TheWitchingHour where
  runMessage msg s@(TheWitchingHour attrs) = case msg of
    StandaloneSetup -> do
      push $ SetChaosTokens (chaosBagContents $ scenarioDifficulty attrs)
      pure s
    PreScenarioSetup -> do
      players <- allPlayers
      lead <- getLeadPlayer

      pushAll
        [ story players intro1
        , chooseOne
            lead
            [ Label
                "“What can I do to avoid this fate?”"
                [SetupStep (toTarget attrs) 2]
            , Label "“This is bullshit.”" [SetupStep (toTarget attrs) 3]
            ]
        , story players intro4
        ]
      pure s
    Setup -> do
      iids <- getInvestigatorIds

      -- The Devourer Below is only locations
      encounterDeck <-
        buildEncounterDeckExcluding
          [Enemies.anetteMason]
          [ EncounterSet.TheWitchingHour
          , EncounterSet.AnettesCoven
          , EncounterSet.CityOfSins
          , EncounterSet.Witchcraft
          , EncounterSet.AncientEvils
          , EncounterSet.StrikingFear
          ]

      agentsOfShubNiggurath <-
        map EncounterCard
          <$> gatherEncounterSet EncounterSet.AgentsOfShubNiggurath
      agentsOfAzathoth <-
        map EncounterCard
          <$> gatherEncounterSet EncounterSet.AgentsOfAzathoth

      witchHauntedWoods <-
        sampleN 5
          $ Locations.witchHauntedWoodsAbandonedMine
          :| [ Locations.witchHauntedWoodsCairnStones
             , Locations.witchHauntedWoodsTheLonelyTree
             , Locations.witchHauntedWoodsChildsTreeHouse
             , Locations.witchHauntedWoodsTaintedWell
             , Locations.witchHauntedWoodsHermitsHouse
             , Locations.witchHauntedWoodsOvergrownBarn
             ]

      setAsideCards <-
        (<> agentsOfShubNiggurath <> agentsOfAzathoth)
          <$> genCards
            [ Enemies.anetteMason
            , Locations.arkhamWoodsUnhallowedGround
            , Locations.arkhamWoodsTwistingPaths
            , Locations.arkhamWoodsOldHouse
            , Locations.arkhamWoodsCliffside
            , Locations.arkhamWoodsTangledThicket
            , Locations.arkhamWoodsQuietGlade
            ]

      let
        woodsWithInvestigators = zip (cycleN 5 iids) witchHauntedWoods
        locationMap =
          foldMap
            ( \(investigator, location) ->
                MonoidalMap.singleton investigator (location :| [])
            )
            woodsWithInvestigators
      startingLocations <-
        Map.fromList
          <$> traverse
            (\(k, v) -> (k,) <$> sample v)
            (MonoidalMap.toList locationMap)

      locationPlacements <-
        concatForM woodsWithInvestigators $ \(investigator, location) -> do
          (lid, placement) <- placeLocationCard location
          let
            mMoveTo = do
              startingLocation <- lookup investigator startingLocations
              guard $ location == startingLocation
              pure $ MoveTo $ move attrs investigator lid

          pure
            $ placement
            : PutLocationInFrontOf investigator lid
            : maybeToList mMoveTo

      pushAll
        $ [SetEncounterDeck encounterDeck, SetAgendaDeck, SetActDeck]
        <> locationPlacements

      agendas <- genCards [Agendas.temperanceXIV, Agendas.theNightHowls]
      acts <-
        genCards
          [ Acts.lostInTheWoods
          , Acts.witchHauntings
          , Acts.pathsIntoTwilight
          , Acts.aCircleUnbroken
          ]

      TheWitchingHour
        <$> runMessage
          msg
          ( attrs
              & (setAsideCardsL .~ setAsideCards)
              & (agendaStackL . at 1 ?~ agendas)
              & (actStackL . at 1 ?~ acts)
          )
    SetupStep (isTarget attrs -> True) 2 -> do
      players <- allPlayers
      lead <- getLead
      -- collection is infinite so we only care if the lead already has either card in their deck
      addCards <-
        fieldMap
          InvestigatorDeck
          ( not
              . any ((`elem` [Assets.theTowerXVI, Assets.aceOfRods1]) . toCardDef)
          )
          lead
      pushAll
        $ [ story players intro2
          , Record YouHaveAcceptedYourFate
          , AddChaosToken Tablet
          , AddChaosToken Tablet
          ]
        <> ( guard addCards
              *> [ AddCampaignCardToDeck lead Assets.theTowerXVI
                 , AddCampaignCardToDeck lead Assets.aceOfRods1
                 ]
           )
      pure s
    SetupStep (isTarget attrs -> True) 3 -> do
      players <- allPlayers
      pushAll
        [ story players intro3
        , Record YouHaveRejectedYourFate
        , AddChaosToken ElderThing
        , AddChaosToken ElderThing
        ]
      pure s
    ScenarioResolution resolution -> do
      players <- allPlayers
      gainXp <- toGainXp attrs $ getXpWithBonus 1
      step <- actStep <$> selectJustField ActSequence AnyAct
      case resolution of
        NoResolution -> do
          push
            $ ScenarioResolution
            $ Resolution
            $ if step == ActStep 4
              then 4
              else 3
        Resolution 1 -> do
          pushAll
            $ [ story players resolution1
              , Record TheWitches'SpellWasBroken
              , recordSetInsert
                  MementosDiscovered
                  [MesmerizingFlute, RitualComponents]
              ]
            <> gainXp
            <> [EndOfGame Nothing]
        Resolution 2 -> do
          pushAll
            $ [ story players resolution2
              , Record TheWitches'SpellWasBroken
              , recordSetInsert
                  MementosDiscovered
                  [MesmerizingFlute, ScrapOfTornShadow]
              ]
            <> gainXp
            <> [EndOfGame Nothing]
        Resolution 3 -> do
          gainXpNoBonus <- toGainXp attrs getXp
          pushAll
            $ [story players resolution3, Record TheWitches'SpellWasCast]
            <> if step == ActStep 3
              then
                recordSetInsert MementosDiscovered [MesmerizingFlute]
                  : gainXp
              else
                gainXpNoBonus
                  <> [EndOfGame Nothing]
        Resolution 4 -> do
          pushAll
            $ [ story players resolution4
              , Record TheWitches'SpellWasCast
              , recordSetInsert MementosDiscovered [MesmerizingFlute]
              ]
            <> gainXp
            <> [EndOfGame Nothing]
        _ -> error "invalid resolution"
      pure s
    _ -> TheWitchingHour <$> runMessage msg attrs
