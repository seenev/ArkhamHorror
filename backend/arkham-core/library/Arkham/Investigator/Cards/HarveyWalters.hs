module Arkham.Investigator.Cards.HarveyWalters (harveyWalters, HarveyWalters (..)) where

import Arkham.Ability
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype HarveyWalters = HarveyWalters InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

harveyWalters :: InvestigatorCard HarveyWalters
harveyWalters =
  investigator HarveyWalters Cards.harveyWalters
    $ Stats {health = 7, sanity = 8, willpower = 4, intellect = 5, combat = 1, agility = 2}

instance HasAbilities HarveyWalters where
  getAbilities (HarveyWalters a) =
    [ playerLimit PerRound
        $ restrictedAbility a 1 (Self <> DuringPhase #investigation)
        $ freeReaction (DrawsCards #after (affectsOthers $ at_ YourLocation) (atLeast 1))
    ]

instance HasChaosTokenValue HarveyWalters where
  getChaosTokenValue iid ElderSign (HarveyWalters attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign $ PositiveModifier 1
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage HarveyWalters where
  runMessage msg i@(HarveyWalters attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 (map windowType -> [Window.DrawCards iid' _]) _ -> do
      push $ drawCards iid' (attrs.ability 1) 1
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      push $ drawCards iid ElderSign 1
      pure i
    _ -> HarveyWalters <$> runMessage msg attrs
