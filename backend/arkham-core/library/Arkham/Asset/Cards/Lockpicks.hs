module Arkham.Asset.Cards.Lockpicks (
  lockpicks,
  lockpicksEffect,
  Lockpicks (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Effect.Runner
import Arkham.Investigate

newtype Lockpicks = Lockpicks AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicks :: AssetCard Lockpicks
lockpicks = asset Lockpicks Cards.lockpicks

instance HasAbilities Lockpicks where
  getAbilities (Lockpicks a) = [investigateAbility a 1 (exhaust a) ControlsThis]

instance RunMessage Lockpicks where
  runMessage msg a@(Lockpicks attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigation <- mkInvestigate sid iid (toAbilitySource attrs 1)

      pushAll
        [ createCardEffect Cards.lockpicks (effectMetaTarget sid) attrs iid
        , toMessage investigation
        ]
      pure a
    _ -> Lockpicks <$> runMessage msg attrs

newtype LockpicksEffect = LockpicksEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lockpicksEffect :: EffectArgs -> LockpicksEffect
lockpicksEffect = cardEffect LockpicksEffect Cards.lockpicks

instance HasModifiersFor LockpicksEffect where
  getModifiersFor target (LockpicksEffect a) | a.target `is` target = do
    pure $ toModifiers a [AddSkillValue #agility]
  getModifiersFor _ _ = pure []

instance RunMessage LockpicksEffect where
  runMessage msg e@(LockpicksEffect attrs) = case msg of
    SkillTestEnds sid _ _ | maybe False (isTarget sid) attrs.metaTarget -> do
      push $ disable attrs
      pure e
    PassedThisSkillTestBy iid _ n | n < 2 -> do
      withSkillTest \sid -> do
        when (maybe False (isTarget sid) attrs.metaTarget) $ do
          let aid = fromJustNote "must be an asset" attrs.source.asset
          pushAll [toDiscardBy iid attrs.source aid, disable attrs]
      pure e
    FailedThisSkillTest iid _ -> do
      withSkillTest \sid -> do
        when (maybe False (isTarget sid) attrs.metaTarget) $ do
          let aid = fromJustNote "must be an asset" attrs.source.asset
          pushAll [toDiscardBy iid attrs.source aid, disable attrs]
      pure e
    _ -> LockpicksEffect <$> runMessage msg attrs
