module Arkham.Agenda.Cards.TemperanceXIV (TemperanceXIV (..), temperanceXIV) where

import Arkham.Agenda.Cards qualified as Cards
import Arkham.Agenda.Runner
import Arkham.Card
import Arkham.Classes
import Arkham.GameValue
import Arkham.Helpers
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Types (Field (..))
import Arkham.Trait (Trait (Witch))

newtype TemperanceXIV = TemperanceXIV AgendaAttrs
  deriving anyclass (IsAgenda, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

temperanceXIV :: AgendaCard TemperanceXIV
temperanceXIV = agenda (1, A) TemperanceXIV Cards.temperanceXIV (Static 8)

instance RunMessage TemperanceXIV where
  runMessage msg a@(TemperanceXIV attrs) = case msg of
    AdvanceAgenda aid | aid == toId attrs && onSide B attrs -> do
      encounterDeck <- scenarioFieldMap ScenarioEncounterDeck unDeck
      let discardAmount = max 0 (length encounterDeck - 5)
      lead <- getLead
      pushAll
        [ DiscardTopOfEncounterDeck lead discardAmount (toSource attrs) (Just $ toTarget attrs)
        , AdvanceAgendaDeck (agendaDeckId attrs) (toSource attrs)
        ]
      pure a
    DiscardedTopOfEncounterDeck _ _ _ (isTarget attrs -> True) -> do
      discard <- scenarioField ScenarioDiscard
      let mWitch = find (`cardMatch` CardWithTrait Witch) discard
      lead <- getLeadPlayer
      for_ mWitch $ \witch -> do
        investigators <- select InvestigatorWithMostCardsInPlayArea
        push
          $ chooseOrRunOne
            lead
            [ targetLabel investigator [InvestigatorDrewEncounterCard investigator witch]
            | investigator <- investigators
            ]
      pure a
    _ -> TemperanceXIV <$> runMessage msg attrs
