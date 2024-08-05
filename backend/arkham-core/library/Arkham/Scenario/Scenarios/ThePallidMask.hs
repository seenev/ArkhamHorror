module Arkham.Scenario.Scenarios.ThePallidMask (
  ThePallidMask (..),
  thePallidMask,
) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Action qualified as Action
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLog
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.Distance
import Arkham.Effect.Window
import Arkham.EffectMetadata
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Helpers.Card
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Label (unLabel)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Projection
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.ScenarioLogKey
import Arkham.Scenarios.ThePallidMask.Helpers
import Arkham.Scenarios.ThePallidMask.Story
import Arkham.SkillTest
import Arkham.Token
import Arkham.Trait (Trait (Geist, Ghoul, Madness, Pact))

newtype ThePallidMask = ThePallidMask ScenarioAttrs
  deriving anyclass IsScenario
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

-- Locations are placed directional, printed on the cards the following
-- directions are possible:
--
-- Left: 2
-- Right: 11
-- Above: 5
-- Below: 6
--
-- This means that our starting position is 2,6 and we have a grid that is
-- 13 x 11, in order to convert we use labels to figure out the new position

thePallidMask :: Difficulty -> ThePallidMask
thePallidMask difficulty =
  scenario
    ThePallidMask
    "03240"
    "The Pallid Mask"
    difficulty
    [ "pos0011 pos0111 pos0211 pos0311 pos0411 pos0511 pos0611 pos0711 pos0811 pos0911 pos1011 pos1111 pos1211 pos1311"
    , "pos0010 pos0110 pos0210 pos0310 pos0410 pos0510 pos0610 pos0710 pos0810 pos0910 pos1010 pos1110 pos1210 pos1310"
    , "pos0009 pos0109 pos0209 pos0309 pos0409 pos0509 pos0609 pos0709 pos0809 pos0909 pos1009 pos1109 pos1209 pos1309"
    , "pos0008 pos0108 pos0208 pos0308 pos0408 pos0508 pos0608 pos0708 pos0808 pos0908 pos1008 pos1108 pos1208 pos1308"
    , "pos0007 pos0107 pos0207 pos0307 pos0407 pos0507 pos0607 pos0707 pos0807 pos0907 pos1007 pos1107 pos1207 pos1307"
    , "pos0006 pos0106 pos0206 pos0306 pos0406 pos0506 pos0606 pos0706 pos0806 pos0906 pos1006 pos1106 pos1206 pos1306"
    , "pos0005 pos0105 pos0205 pos0305 pos0405 pos0505 pos0605 pos0705 pos0805 pos0905 pos1005 pos1105 pos1205 pos1305"
    , "pos0004 pos0104 pos0204 pos0304 pos0404 pos0504 pos0604 pos0704 pos0804 pos0904 pos1004 pos1104 pos1204 pos1304"
    , "pos0003 pos0103 pos0203 pos0303 pos0403 pos0503 pos0603 pos0703 pos0803 pos0903 pos1003 pos1103 pos1203 pos1303"
    , "pos0002 pos0102 pos0202 pos0302 pos0402 pos0502 pos0602 pos0702 pos0802 pos0902 pos1002 pos1102 pos1202 pos1302"
    , "pos0001 pos0101 pos0201 pos0301 pos0401 pos0501 pos0601 pos0701 pos0801 pos0901 pos1001 pos1101 pos1201 pos1301"
    , "pos0000 pos0100 pos0200 pos0300 pos0400 pos0500 pos0600 pos0700 pos0800 pos0900 pos1000 pos1100 pos1200 pos1300"
    ]

instance HasModifiersFor ThePallidMask where
  getModifiersFor (InvestigatorTarget iid) (ThePallidMask a) = do
    extraXp <- elem (recorded $ unInvestigatorId iid) <$> getRecordSet ReadActII
    pure $ toModifiers a [XPModifier 2 | extraXp]
  getModifiersFor _ _ = pure []

standaloneCampaignLog :: CampaignLog
standaloneCampaignLog =
  mkCampaignLog
    { campaignLogRecorded =
        setFromList
          [YouFoundNigelsHome, YouEnteredTheCatacombsOnYourOwn]
    }

instance HasChaosTokenValue ThePallidMask where
  getChaosTokenValue iid chaosTokenFace (ThePallidMask attrs) = case chaosTokenFace of
    Skull -> do
      -- -X where X is the number of locations away from the starting location
      startingLocation <-
        selectJust
          $ LocationWithLabel
          $ positionToLabel
            startPosition
      yourLocation <-
        fromJustNote "no location" <$> field InvestigatorLocation iid
      distance <-
        unDistance
          . fromMaybe (Distance 0)
          <$> getDistance startingLocation yourLocation
      pure $ toChaosTokenValue attrs Skull (min 5 distance) distance
    Cultist -> pure $ toChaosTokenValue attrs Cultist 2 3
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
  , MinusOne
  , MinusTwo
  , MinusTwo
  , MinusThree
  , MinusThree
  , MinusFour
  , Skull
  , Skull
  , Skull
  , AutoFail
  , ElderSign
  ]

instance RunMessage ThePallidMask where
  runMessage msg s@(ThePallidMask attrs) = case msg of
    StandaloneSetup -> do
      leadInvestigatorId <- getLeadInvestigatorId
      randomToken <- sample (Cultist :| [Tablet, ElderThing])
      pushAll
        [ SetChaosTokens $ standaloneChaosTokens <> [randomToken, randomToken]
        , AddCampaignCardToDeck leadInvestigatorId Enemies.theManInThePallidMask
        ]
      pure
        . ThePallidMask
        $ attrs
        & standaloneCampaignLogL
        .~ standaloneCampaignLog
    Setup -> do
      players <- allPlayers
      didNotEscapeGazeOfThePhantom <-
        getHasRecord
          YouDidNotEscapeTheGazeOfThePhantom
      unableToFindNigel <- getHasRecord YouWereUnableToFindNigel

      let
        awokeInsideTheCatacombs =
          didNotEscapeGazeOfThePhantom || unableToFindNigel
        intro = if awokeInsideTheCatacombs then intro1 else intro2

      harukoInterviewed <-
        elem (recorded $ toCardCode Assets.ishimaruHaruko)
          <$> getRecordSet VIPsInterviewed

      encounterDeck <-
        buildEncounterDeck
          [ EncounterSet.ThePallidMask
          , EncounterSet.Ghouls
          , EncounterSet.Hauntings
          , EncounterSet.ChillingCold
          ]

      theGateToHell <- genCard Locations.theGateToHell
      blockedPassage <- genCard Locations.blockedPassage
      tombOfShadows <- genCard Locations.tombOfShadows

      otherCatacombs <-
        genCards
          [ Locations.stoneArchways
          , Locations.stoneArchways
          , Locations.cryptOfTheSepulchralLamp
          , Locations.boneFilledCaverns
          , Locations.wellOfSouls
          , Locations.candlelitTunnels
          , Locations.candlelitTunnels
          , Locations.labyrinthOfBones
          , Locations.labyrinthOfBones
          , Locations.narrowShaft
          , Locations.shiveringPools
          ]

      (startingLocation, remainingCatacombs) <-
        if awokeInsideTheCatacombs
          then do
            shuffled <- shuffleM (theGateToHell : otherCatacombs)
            case shuffled of
              (x : xs) -> pure (x, xs)
              _ -> error "invalid setup"
          else (theGateToHell,) <$> shuffleM otherCatacombs

      (startingLocationId, placeStartingLocation) <-
        placeLocation
          startingLocation

      theManInThePallidMask <-
        getCampaignStoryCard
          Enemies.theManInThePallidMask

      let (bottom3, rest) = splitAt 3 remainingCatacombs
      bottom <- shuffleM ([tombOfShadows, blockedPassage] <> bottom3)
      let catacombsDeck = rest <> bottom

      pushAll
        $ [story players intro]
        <> [story players harukosInformation | harukoInterviewed]
        <> [Remember YouOpenedASecretPassageway | harukoInterviewed]
        <> [ SetEncounterDeck encounterDeck
           , SetAgendaDeck
           , SetActDeck
           , placeStartingLocation
           , SetLocationLabel
              startingLocationId
              (unLabel $ positionToLabel startPosition)
           , PlaceTokens (toSource attrs) (toTarget startingLocationId) Resource 1
           , MoveAllTo (toSource attrs) startingLocationId
           , SetupStep (toTarget attrs) 1
           , RemoveFromBearersDeckOrDiscard theManInThePallidMask
           ]
      agendas <- genCards [Agendas.empireOfTheDead, Agendas.empireOfTheUndead]
      acts <-
        genCards
          [ Acts.throughTheCatacombs
          , Acts.thePathIsBarred
          , Acts.theWayOut
          , Acts.leadingTheWay
          ]

      ThePallidMask
        <$> runMessage
          msg
          ( attrs
              & (decksL . at CatacombsDeck ?~ catacombsDeck)
              & (setAsideCardsL .~ [PlayerCard theManInThePallidMask])
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    SetupStep (isTarget attrs -> True) 1 -> do
      lead <- getLeadPlayer
      leadId <- getLeadInvestigatorId
      catacombs <- select UnrevealedLocation
      youOpenedASecretPassageway <- remembered YouOpenedASecretPassageway
      pushWhen youOpenedASecretPassageway
        $ chooseOne lead
        $ [ targetLabel catacomb [RevealLocation (Just leadId) catacomb]
          | catacomb <- catacombs
          ]
      pure s
    ResolveChaosToken t Cultist iid -> do
      mAction <- getSkillTestAction
      case mAction of
        Just Action.Fight ->
          withSkillTest \sid ->
            push
              $ CreateWindowModifierEffect
                (EffectSkillTestWindow sid)
                ( EffectModifiers
                    $ toModifiers
                      attrs
                      [ if isEasyStandard attrs
                          then DamageDealt (-1)
                          else NoDamageDealt
                      ]
                )
                (ChaosTokenSource t)
                (InvestigatorTarget iid)
        _ -> pure ()
      pure s
    ResolveChaosToken _ Tablet iid -> do
      player <- getPlayer iid
      if isEasyStandard attrs
        then do
          enemies <-
            select
              (ReadyEnemy <> EnemyOneOf (map EnemyWithTrait [Ghoul, Geist]))
          unless (null enemies)
            $ push
            $ chooseOne
              player
              [ targetLabel enemy [InitiateEnemyAttack $ enemyAttack enemy attrs iid]
              | enemy <- enemies
              ]
        else do
          enemies <-
            select
              (ReadyEnemy <> EnemyOneOf (map EnemyWithTrait [Ghoul, Geist]))
          unless (null enemies)
            $ push
            $ chooseOne
              player
              [ targetLabel
                enemy
                [ Ready (EnemyTarget enemy)
                , InitiateEnemyAttack $ enemyAttack enemy attrs iid
                ]
              | enemy <- enemies
              ]
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ -> case chaosTokenFace token of
      ElderThing -> do
        push
          $ findAndDrawEncounterCard iid
          $ CardWithType EnemyType
          <> CardWithOneOf (map CardWithTrait [Ghoul, Geist])
        pure s
      _ -> pure s
    ScenarioResolution res -> do
      investigatorIds <- allInvestigatorIds
      players <- allPlayers
      lead <- getLeadPlayer
      harukoSlain <-
        selectOne
          (VictoryDisplayCardMatch $ cardIs Enemies.ishimaruHaruko)
      chasingTheStrangerTallies <- getRecordCount ChasingTheStranger
      let
        updateSlain =
          [ recordSetInsert VIPsSlain [toCardCode haruko]
          | haruko <- maybeToList harukoSlain
          ]
        (token, story') = case res of
          NoResolution -> (ElderThing, noResolution)
          Resolution 1 -> (Cultist, resolution1)
          Resolution 2 -> (Tablet, resolution2)
          _ -> error "Invalid resolution"
      pushAll
        $ [story players story', Record YouKnowTheSiteOfTheGate]
        <> [ chooseSome
              lead
              "Done having investigators read Act II"
              [ targetLabel
                iid
                [ recordSetInsert ReadActII [unInvestigatorId iid]
                , SearchCollectionForRandom
                    iid
                    (toSource attrs)
                    ( BasicWeaknessCard
                        <> CardWithOneOf (map CardWithTrait [Madness, Pact])
                    )
                ]
              | iid <- investigatorIds
              ]
           ]
        <> [ RemoveAllChaosTokens Cultist
           , RemoveAllChaosTokens Tablet
           , RemoveAllChaosTokens ElderThing
           , AddChaosToken token
           , AddChaosToken token
           ]
        <> [ RecordCount ChasingTheStranger (chasingTheStrangerTallies + 2)
           | res == Resolution 2
           ]
        <> updateSlain
        <> [ScenarioResolutionStep 1 res]
      pure s
    ScenarioResolutionStep 1 _ -> do
      gainXp <- toGainXp attrs getXp
      pushAll $ gainXp <> [EndOfGame Nothing]
      pure s
    RequestedPlayerCard iid source mcard _ | isSource attrs source -> do
      for_ mcard $ push . AddCardToDeckForCampaign iid
      pure s
    _ -> ThePallidMask <$> runMessage msg attrs
