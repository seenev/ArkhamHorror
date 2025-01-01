module Arkham.Investigator.Cards.MarkHarrigan (markHarrigan, MarkHarrigan (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Assets
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Import.Lifted
import Arkham.Matcher

newtype MarkHarrigan = MarkHarrigan InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

markHarrigan :: InvestigatorCard MarkHarrigan
markHarrigan =
  startsWith [Assets.sophieInLovingMemory]
    $ investigator MarkHarrigan Cards.markHarrigan
    $ Stats {willpower = 3, intellect = 2, combat = 5, agility = 3, health = 9, sanity = 5}

instance HasAbilities MarkHarrigan where
  getAbilities (MarkHarrigan attrs) =
    [ playerLimit PerPhase
        $ restrictedAbility attrs 1 Self
        $ freeReaction
        $ PlacedToken #after AnySource (TargetControlledBy You) #damage
    ]

instance HasChaosTokenValue MarkHarrigan where
  getChaosTokenValue iid ElderSign (MarkHarrigan attrs) | attrs `is` iid = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier attrs.healthDamage
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage MarkHarrigan where
  runMessage msg i@(MarkHarrigan attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      drawCardsIfCan iid (attrs.ability 1) 1
      pure i
    _ -> MarkHarrigan <$> liftRunMessage msg attrs
