module Arkham.Story.Cards.CylindersOfKadatheron (CylindersOfKadatheron (..), cylindersOfKadatheron) where

import Arkham.Deck qualified as Deck
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Matcher
import Arkham.Prelude
import Arkham.ScenarioLogKey
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype CylindersOfKadatheron = CylindersOfKadatheron StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cylindersOfKadatheron :: StoryCard CylindersOfKadatheron
cylindersOfKadatheron = story CylindersOfKadatheron Cards.cylindersOfKadatheron

instance RunMessage CylindersOfKadatheron where
  runMessage msg s@(CylindersOfKadatheron attrs) = case msg of
    ResolveStory _ ResolveIt story' | story' == toId attrs -> do
      tenebrousNightgaunts <- getSetAsideCardsMatching $ cardIs Enemies.tenebrousNightgaunt
      pushAll
        $ ScenarioCountIncrementBy SignOfTheGods 1
        : [ShuffleCardsIntoDeck Deck.EncounterDeck [card] | card <- take 1 tenebrousNightgaunts]
      pure s
    _ -> CylindersOfKadatheron <$> runMessage msg attrs
