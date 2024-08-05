module Arkham.Location.Cards.ForsakenTowerOfInfiniteTruth (
  forsakenTowerOfInfiniteTruth,
  ForsakenTowerOfInfiniteTruth (..),
)
where

import Arkham.Action qualified as Action
import Arkham.Attack
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Scenarios.WhereTheGodsDwell.Helpers

newtype ForsakenTowerOfInfiniteTruth = ForsakenTowerOfInfiniteTruth LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

forsakenTowerOfInfiniteTruth :: LocationCard ForsakenTowerOfInfiniteTruth
forsakenTowerOfInfiniteTruth =
  location
    ForsakenTowerOfInfiniteTruth
    Cards.forsakenTowerOfInfiniteTruth
    2
    (PerPlayer 1)

instance HasAbilities ForsakenTowerOfInfiniteTruth where
  getAbilities (ForsakenTowerOfInfiniteTruth attrs) =
    extendRevealed attrs $ forsakenTowerAbilities attrs

instance RunMessage ForsakenTowerOfInfiniteTruth where
  runMessage msg l@(ForsakenTowerOfInfiniteTruth attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      revealWhisperingChaos attrs
      nyarlathoteps <- select $ EnemyInHandOf $ InvestigatorWithId iid
      chooseOne
        iid
        [ targetLabel
          nyarlathotep
          [HandleTargetChoice iid (attrs.ability 1) (toTarget nyarlathotep)]
        | nyarlathotep <- nyarlathoteps
        ]
      pure l
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) (EnemyTarget nyarlathotep) -> do
      sid <- getRandom
      push $ EvadeEnemy sid iid nyarlathotep (attrs.ability 1) (Just $ toTarget attrs) #willpower False
      pure l
    Successful (Action.Evade, EnemyTarget eid) _iid _ (isTarget attrs -> True) _ -> do
      discardWhisperingChaos attrs
      push $ AddToVictory (toTarget eid)
      pure l
    Failed (Action.Evade, EnemyTarget eid) iid _ (isTarget attrs -> True) _ -> do
      shuffleWhisperingChaosBackIntoEncounterDeck attrs
      pushAll
        [ InitiateEnemyAttack $ enemyAttack eid (attrs.ability 1) iid
        , ShuffleBackIntoEncounterDeck (toTarget eid)
        ]
      pure l
    _ -> ForsakenTowerOfInfiniteTruth <$> liftRunMessage msg attrs
