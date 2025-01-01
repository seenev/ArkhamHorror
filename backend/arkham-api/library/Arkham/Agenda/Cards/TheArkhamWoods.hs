module Arkham.Agenda.Cards.TheArkhamWoods (
  TheArkhamWoods (..),
  theArkhamWoods,
) where

import Arkham.Prelude

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Helpers
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Matcher
import Arkham.Trait

newtype TheArkhamWoods = TheArkhamWoods AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theArkhamWoods :: AgendaCard TheArkhamWoods
theArkhamWoods = agenda (1, A) TheArkhamWoods Cards.theArkhamWoods (Static 4)

instance RunMessage TheArkhamWoods where
  runMessage msg a@(TheArkhamWoods attrs) = case msg of
    AdvanceAgenda aid | aid == toId a && onSide B attrs -> do
      lead <- getLead
      pushAll
        [ ShuffleEncounterDiscardBackIn
        , DiscardUntilFirst
            lead
            (toSource aid)
            Deck.EncounterDeck
            (BasicCardMatch $ CardWithType EnemyType <> CardWithTrait Monster)
        ]
      pure a
    RequestedEncounterCard (isSource attrs -> True) _ mcard -> do
      case mcard of
        Nothing -> push $ advanceAgendaDeck attrs
        Just card -> do
          mainPath <- getJustLocationByName "Main Path"
          pushAll
            [ SpawnEnemyAt (EncounterCard card) mainPath
            , PlaceDoom (toSource attrs) (CardIdTarget $ toCardId card) 1
            , advanceAgendaDeck attrs
            ]
      pure a
    _ -> TheArkhamWoods <$> runMessage msg attrs
