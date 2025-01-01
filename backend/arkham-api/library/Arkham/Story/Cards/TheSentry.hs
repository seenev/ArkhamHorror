module Arkham.Story.Cards.TheSentry (TheSentry (..), theSentry) where

import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers (perPlayer)
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Message.Lifted hiding (story)
import Arkham.Message.Lifted.CreateEnemy
import Arkham.Prelude
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner hiding (createEnemy, createEnemyWith)

newtype TheSentry = TheSentry StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theSentry :: StoryCard TheSentry
theSentry = story TheSentry Cards.theSentry

instance RunMessage TheSentry where
  runMessage msg s@(TheSentry attrs) = runQueueT $ case msg of
    ResolveStory iid ResolveIt story' | story' == toId attrs -> do
      -- Spawn the set-aside Gug Sentinel enemy at this location, with 1  clues
      -- on it. Test  (3). If you succeed, Gug Sentinel enters play exhausted
      -- and unengaged. Otherwise, it enters play engaged with you.
      sid <- getRandom
      beginSkillTest sid iid attrs iid #agility (Fixed 3)
      pure s
    FailedThisSkillTest iid (isSource attrs -> True) -> do
      gugSentinelCard <- getSetAsideCard Enemies.gugSentinel
      runCreateEnemyT gugSentinelCard iid \sentinel -> do
        afterCreate $ placeClues attrs sentinel =<< perPlayer 1
      pure s
    PassedThisSkillTest _ (isSource attrs -> True) -> do
      cityOfGugs <- selectJust $ locationIs Locations.cityOfGugs
      gugSentinelCard <- getSetAsideCard Enemies.gugSentinel
      runCreateEnemyT gugSentinelCard cityOfGugs \sentinel -> do
        createExhausted
        placeClues attrs sentinel =<< perPlayer 1
      pure s
    _ -> TheSentry <$> liftRunMessage msg attrs
