module Arkham.Scenario.Scenarios.TheMiskatonicMuseum where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Acts
import Arkham.Agenda.Cards qualified as Agendas
import Arkham.Asset.Cards qualified as Assets
import Arkham.Attack
import Arkham.CampaignLogKey
import Arkham.Card
import Arkham.ChaosToken
import Arkham.Classes
import Arkham.Difficulty
import Arkham.EncounterSet qualified as EncounterSet
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher hiding (RevealLocation)
import Arkham.Message
import Arkham.Name
import Arkham.Resolution
import Arkham.Scenario.Helpers
import Arkham.Scenario.Runner
import Arkham.Scenarios.TheMiskatonicMuseum.Helpers
import Arkham.Scenarios.TheMiskatonicMuseum.Story
import Arkham.Treachery.Cards qualified as Treacheries
import Arkham.Zone

newtype TheMiskatonicMuseum = TheMiskatonicMuseum ScenarioAttrs
  deriving stock Generic
  deriving anyclass (IsScenario, HasModifiersFor)
  deriving newtype (Show, ToJSON, FromJSON, Entity, Eq)

theMiskatonicMuseum :: Difficulty -> TheMiskatonicMuseum
theMiskatonicMuseum difficulty =
  scenario
    TheMiskatonicMuseum
    "02118"
    "The Miskatonic Museum"
    difficulty
    [ ".     .     .                    .                    hall3 hall3          hall4          hall4 .                  .              .     ."
    , ".     .     hall2                hall2                hall3 hall3          hall4          hall4 hall5              hall5          .     ."
    , "hall1 hall1 hall2                hall2                .     museumHalls    museumHalls    .     hall5              hall5          hall6 hall6"
    , "hall1 hall1 .                    .                    .     museumHalls    museumHalls    .     .                  .              hall6 hall6"
    , ".     .     administrationOffice administrationOffice .     museumEntrance museumEntrance .     securityOffice     securityOffice .     ."
    , ".     .     administrationOffice administrationOffice .     museumEntrance museumEntrance .     securityOffice     securityOffice .     ."
    ]

instance HasChaosTokenValue TheMiskatonicMuseum where
  getChaosTokenValue iid chaosTokenFace (TheMiskatonicMuseum attrs) = case chaosTokenFace of
    Skull -> do
      huntingHorrorAtYourLocation <-
        selectAny
          $ enemyIs Enemies.huntingHorror
          <> EnemyAt
            (LocationWithInvestigator $ InvestigatorWithId iid)
      pure
        $ if huntingHorrorAtYourLocation
          then toChaosTokenValue attrs Skull 3 4
          else toChaosTokenValue attrs Skull 1 2
    Cultist -> pure $ toChaosTokenValue attrs Cultist 1 3
    Tablet -> pure $ toChaosTokenValue attrs Tablet 2 4
    ElderThing -> pure $ toChaosTokenValue attrs ElderThing 3 5
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
  , MinusFour
  , Skull
  , Skull
  , Cultist
  , Tablet
  , ElderThing
  , AutoFail
  , ElderSign
  ]

instance RunMessage TheMiskatonicMuseum where
  runMessage msg s@(TheMiskatonicMuseum attrs@ScenarioAttrs {..}) = case msg of
    StandaloneSetup -> do
      push (SetChaosTokens standaloneChaosTokens)
      pure s
    LookAtTopOfDeck iid ScenarioDeckTarget n -> do
      case fromJustNote "must be set" (lookup ExhibitDeck scenarioDecks) of
        xs -> do
          let cards = take n xs
          player <- getPlayer iid
          pushAll
            [FocusCards cards, chooseOne player [Label "Continue" [UnfocusCards]]]
      pure s
    Setup -> do
      players <- allPlayers

      armitageKidnapped <-
        getHasRecordOrStandalone
          DrHenryArmitageWasKidnapped
          True

      exhibitHalls <-
        shuffleM
          =<< genCards
            [ Locations.exhibitHallAthabaskanExhibit
            , Locations.exhibitHallMedusaExhibit
            , Locations.exhibitHallNatureExhibit
            , Locations.exhibitHallEgyptianExhibit
            , Locations.exhibitHallHallOfTheDead
            ]

      let (bottom, top) = splitAt 2 exhibitHalls

      restrictedHall <- genCard Locations.exhibitHallRestrictedHall

      bottom' <- shuffleM $ restrictedHall : bottom

      let exhibitDeck = top <> bottom'

      encounterDeck <-
        buildEncounterDeckExcluding
          [Treacheries.shadowSpawned, Assets.haroldWalsted, Assets.adamLynch]
          [ EncounterSet.TheMiskatonicMuseum
          , EncounterSet.BadLuck
          , EncounterSet.Sorcery
          , EncounterSet.TheBeyond
          , EncounterSet.ChillingCold
          , EncounterSet.LockedDoors
          ]

      (museumEntranceId, placeMuseumEntrance) <-
        placeLocationCard
          Locations.museumEntrance
      placeMuseumHalls <- placeLocationCard_ Locations.museumHalls
      placeSecurityOffice <-
        placeLocationCard_
          =<< sample
            (Locations.securityOffice_128 :| [Locations.securityOffice_129])
      placeAdministrationOffice <-
        placeLocationCard_
          =<< sample
            ( Locations.administrationOffice_130
                :| [Locations.administrationOffice_131]
            )

      pushAll
        [ story players intro1
        , story players (intro2 armitageKidnapped)
        , SetEncounterDeck encounterDeck
        , SetAgendaDeck
        , SetActDeck
        , placeSecurityOffice
        , placeAdministrationOffice
        , placeMuseumEntrance
        , placeMuseumHalls
        , RevealLocation Nothing museumEntranceId
        , MoveAllTo (toSource attrs) museumEntranceId
        ]

      setAsideCards <-
        genCards
          [ Assets.haroldWalsted
          , Assets.adamLynch
          , Assets.theNecronomiconOlausWormiusTranslation
          , Treacheries.shadowSpawned
          ]

      agendas <-
        genCards
          [Agendas.restrictedAccess, Agendas.shadowsDeepen, Agendas.inEveryShadow]
      acts <-
        genCards
          [ Acts.findingAWayInside
          , Acts.nightAtTheMuseum
          , Acts.breakingAndEntering
          , Acts.searchingForTheTome
          ]

      TheMiskatonicMuseum
        <$> runMessage
          msg
          ( attrs
              & (decksL . at ExhibitDeck ?~ exhibitDeck)
              & (setAsideCardsL .~ setAsideCards)
              & (actStackL . at 1 ?~ acts)
              & (agendaStackL . at 1 ?~ agendas)
          )
    PlacedLocation name _ lid -> do
      when (nameTitle name == "Exhibit Hall") $ do
        hallCount <- selectCount $ LocationWithTitle "Exhibit Hall"
        push (SetLocationLabel lid $ "hall" <> tshow hallCount)
      pure s
    ResolveChaosToken _ Tablet iid | isEasyStandard attrs -> do
      s <$ push (InvestigatorPlaceCluesOnLocation iid (ChaosTokenEffectSource Tablet) 1)
    ResolveChaosToken _ Tablet iid | isHardExpert attrs -> do
      lid <- getJustLocation iid
      mHuntingHorrorId <- getHuntingHorrorWith $ EnemyAt $ LocationWithId lid
      for_ mHuntingHorrorId $ \huntingHorrorId ->
        push (EnemyAttack $ enemyAttack huntingHorrorId attrs iid)
      pure s
    FailedSkillTest iid _ _ (ChaosTokenTarget token) _ _ ->
      s <$ case chaosTokenFace token of
        Cultist ->
          push
            $ FindEncounterCard
              iid
              (toTarget attrs)
              [FromEncounterDeck, FromEncounterDiscard, FromVoid]
              (cardIs Enemies.huntingHorror)
        ElderThing ->
          push
            $ ChooseAndDiscardAsset iid (ChaosTokenEffectSource ElderThing) AnyAsset
        _ -> pure ()
    FoundEncounterCard iid target ec | isTarget attrs target -> do
      lid <- getJustLocation iid
      s <$ push (SpawnEnemyAt (EncounterCard ec) lid)
    FoundEnemyInVoid iid target eid | isTarget attrs target -> do
      lid <- getJustLocation iid
      s <$ push (EnemySpawnFromVoid Nothing lid eid)
    ScenarioResolution NoResolution -> do
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players noResolution
          , Record TheInvestigatorsFailedToRecoverTheNecronomicon
          ]
        <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 1) -> do
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players resolution1
          , Record TheInvestigatorsDestroyedTheNecronomicon
          ]
        <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    ScenarioResolution (Resolution 2) -> do
      lead <- getLeadPlayer
      investigatorIds <- allInvestigatorIds
      players <- allPlayers
      xp <- getXp
      pushAll
        $ [ story players resolution2
          , Record TheInvestigatorsTookCustodyOfTheNecronomicon
          , chooseOne
              lead
              [ Label
                  "Add The Necronomicon (Olaus Wormius Translation) to a deck"
                  [ chooseOne
                      lead
                      [ targetLabel
                        iid
                        [ AddCampaignCardToDeck
                            iid
                            Assets.theNecronomiconOlausWormiusTranslation
                        ]
                      | iid <- investigatorIds
                      ]
                  ]
              , Label
                  "Do not add The Necronomicon (Olaus Wormius Translation) to a deck"
                  []
              ]
          , AddChaosToken ElderThing
          ]
        <> [GainXP iid (toSource attrs) n | (iid, n) <- xp]
        <> [EndOfGame Nothing]
      pure s
    _ -> TheMiskatonicMuseum <$> runMessage msg attrs
