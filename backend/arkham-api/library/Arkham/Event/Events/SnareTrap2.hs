module Arkham.Event.Events.SnareTrap2 (snareTrap2, SnareTrap2 (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Placement
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype SnareTrap2 = SnareTrap2 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

snareTrap2 :: EventCard SnareTrap2
snareTrap2 = event SnareTrap2 Cards.snareTrap2

instance HasAbilities SnareTrap2 where
  getAbilities (SnareTrap2 a) = case eventAttachedTarget a of
    Just (LocationTarget lid) ->
      [mkAbility a 1 $ forced $ EnemyEnters #after (LocationWithId lid) NonEliteEnemy]
    Just (EnemyTarget eid) ->
      [mkAbility a 2 $ forced $ EnemyWouldReady #when $ EnemyWithId eid]
    _ -> []

instance RunMessage SnareTrap2 where
  runMessage msg e@(SnareTrap2 attrs) = case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      lid <- getJustLocation iid
      push $ PlaceEvent eid (AttachedToLocation lid)
      pure e
    UseCardAbility _iid (isSource attrs -> True) 1 [windowType -> Window.EnemyEnters enemyId _] _ -> do
      iids <- select $ investigatorEngagedWith enemyId
      pushAll
        $ Exhaust (EnemyTarget enemyId)
        : map (`DisengageEnemy` enemyId) iids
          <> [PlaceEvent (toId attrs) (AttachedToEnemy enemyId)]
      pure e
    UseCardAbility _ (isSource attrs -> True) 2 [windowType -> Window.WouldReady target] _ -> do
      replaceMessageMatching
        \case
          Ready t -> t == target
          _ -> False
        (const [toDiscardBy attrs.owner (attrs.ability 2) attrs])
      pure e
    _ -> SnareTrap2 <$> runMessage msg attrs
