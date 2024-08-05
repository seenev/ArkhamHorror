module Arkham.Helpers.Deck where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes.HasGame
import Arkham.Deck qualified as Deck
import Arkham.Helpers
import Arkham.Helpers.Scenario
import Arkham.Id
import Arkham.Investigator.Types (Field (..))
import Arkham.Message
import Arkham.Projection
import Arkham.Scenario.Deck
import Arkham.Scenario.Types (Field (..))
import Arkham.Target
import Control.Lens (non, _1)
import Data.Map.Strict qualified as Map

withDeck :: ([a] -> [a]) -> Deck a -> Deck a
withDeck f (Deck xs) = Deck (f xs)

withDeckM :: Functor f => ([a] -> f [a]) -> Deck a -> f (Deck a)
withDeckM f (Deck xs) = Deck <$> f xs

removeEachFromDeck :: HasCardDef a => Deck a -> [CardDef] -> Deck a
removeEachFromDeck deck removals = flip withDeck deck $ \cards ->
  foldl' (\cs m -> deleteFirstMatch ((== m) . toCardDef) cs) cards removals

getDeck :: HasGame m => Deck.DeckSignifier -> m [Card]
getDeck = \case
  Deck.InvestigatorDeck iid -> fieldMap InvestigatorDeck (map PlayerCard . unDeck) iid
  Deck.InvestigatorDiscard iid -> fieldMap InvestigatorDiscard (map PlayerCard) iid
  Deck.EncounterDeck -> scenarioFieldMap ScenarioEncounterDeck (map EncounterCard . unDeck)
  Deck.EncounterDiscard -> scenarioFieldMap ScenarioDiscard (map EncounterCard)
  Deck.ScenarioDeckByKey k -> scenarioFieldMap ScenarioDecks (Map.findWithDefault [] k)
  Deck.InvestigatorDeckByKey iid k -> fieldMap InvestigatorDecks (Map.findWithDefault [] k) iid
  Deck.EncounterDeckByKey k -> case k of
    RegularEncounterDeck -> scenarioFieldMap ScenarioEncounterDeck (map EncounterCard . unDeck)
    other ->
      scenarioFieldMap
        ScenarioEncounterDecks
        (map EncounterCard . unDeck . view (at other . non (Deck [], []) . _1))

initDeckTrauma
  :: MonadRandom m => Deck PlayerCard -> InvestigatorId -> PlayerId -> Target -> m [Message]
initDeckTrauma deck' iid pid target = do
  let
    toMentalTrauma = \case
      PurchaseMentalTrauma n -> n
      _ -> 0
    toPhysicalTrauma = \case
      PurchasePhysicalTrauma n -> n
      _ -> 0
    toAnyTrauma = \case
      PurchaseAnyTrauma n -> n
      _ -> 0
    getResult (a, b, c) = (getSum a, getSum b, getSum c)

    (physicalTrauma, mentalTrauma, anyTrauma) =
      getResult
        $ foldMap
          ( \(toCardDef -> cdPurchaseTrauma -> t) -> (Sum $ toPhysicalTrauma t, Sum $ toMentalTrauma t, Sum $ toAnyTrauma t)
          )
          deck'
  chooseMsg <-
    chooseAmounts
      pid
      ("Suffer " <> tshow anyTrauma <> " total physical and/or mental trauma")
      (TotalAmountTarget anyTrauma)
      [("Physical", (0, anyTrauma)), ("Mental", (0, anyTrauma))]
      (LabeledTarget "Purchase Trauma" target)
  pure
    $ [SufferTrauma iid physicalTrauma mentalTrauma | mentalTrauma > 0 || physicalTrauma > 0]
    <> [chooseMsg | anyTrauma > 0]
