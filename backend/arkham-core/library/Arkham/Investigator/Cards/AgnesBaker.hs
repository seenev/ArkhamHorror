module Arkham.Investigator.Cards.AgnesBaker (
  AgnesBaker (..),
  agnesBaker,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher hiding (NonAttackDamageEffect)

newtype AgnesBaker = AgnesBaker InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock (Data)

agnesBaker :: InvestigatorCard AgnesBaker
agnesBaker =
  investigator AgnesBaker Cards.agnesBaker
    $ Stats {health = 6, sanity = 8, willpower = 5, intellect = 2, combat = 2, agility = 3}

instance HasAbilities AgnesBaker where
  getAbilities (AgnesBaker x) =
    [ playerLimit PerPhase
        $ restrictedAbility x 1 (Self <> CanDealDamage <> enemyExists (EnemyAt YourLocation))
        $ freeReaction (PlacedCounter #when You AnySource HorrorCounter $ atLeast 1)
    ]

instance HasChaosTokenValue AgnesBaker where
  getChaosTokenValue iid ElderSign (AgnesBaker attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier attrs.sanityDamage
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage AgnesBaker where
  runMessage msg i@(AgnesBaker attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      targets <- select $ enemyAtLocationWith iid <> EnemyCanBeDamagedBySource source
      player <- getPlayer iid
      push $ chooseOne player $ targetLabels targets (only . nonAttackEnemyDamage source 1)
      pure i
    _ -> AgnesBaker <$> runMessage msg attrs
