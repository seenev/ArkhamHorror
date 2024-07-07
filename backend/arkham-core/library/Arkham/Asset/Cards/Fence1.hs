module Arkham.Asset.Cards.Fence1 (
  fence1,
  fence1Effect,
  Fence1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Matcher hiding (DuringTurn)
import Arkham.Matcher qualified as Matcher
import Arkham.Timing qualified as Timing
import Arkham.Trait
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype Fence1 = Fence1 AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fence1 :: AssetCard Fence1
fence1 = asset Fence1 Cards.fence1

instance HasModifiersFor Fence1 where
  getModifiersFor (InvestigatorTarget iid) (Fence1 a) | controlledBy a iid && not (assetExhausted a) = do
    pure
      $ toModifiers a
      $ [CanBecomeFast (CardWithTrait Illicit), CanReduceCostOf (CardWithTrait Illicit <> FastCard) 1]
  getModifiersFor _ _ = pure []

instance HasAbilities Fence1 where
  getAbilities (Fence1 a) =
    [ restrictedAbility a 1 (ControlsThis <> DuringTurn You)
        $ ReactionAbility (Matcher.PlayCard #when You (BasicCardMatch $ CardWithTrait Illicit))
        $ exhaust a
    ]

instance RunMessage Fence1 where
  runMessage msg a@(Fence1 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 [Window Timing.When (Window.PlayCard _ card) _] _ -> do
      let source = toAbilitySource attrs 1
      push
        $ if isFastCard card
          then costModifier source iid (ReduceCostOf (CardWithId $ toCardId card) 1)
          else createCardEffect Cards.fence1 Nothing source (CardIdTarget $ toCardId card)
      pure a
    _ -> Fence1 <$> runMessage msg attrs

newtype Fence1Effect = Fence1Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fence1Effect :: EffectArgs -> Fence1Effect
fence1Effect = cardEffect Fence1Effect Cards.fence1

instance HasModifiersFor Fence1Effect where
  getModifiersFor target (Fence1Effect attrs) | target == effectTarget attrs = do
    pure $ toModifiers attrs [BecomesFast FastPlayerWindow]
  getModifiersFor _ _ = pure []

instance RunMessage Fence1Effect where
  runMessage msg e@(Fence1Effect attrs) = case msg of
    CardEnteredPlay _ card | CardIdTarget (toCardId card) == effectTarget attrs -> do
      push (DisableEffect $ toId attrs)
      pure e
    _ -> Fence1Effect <$> runMessage msg attrs
