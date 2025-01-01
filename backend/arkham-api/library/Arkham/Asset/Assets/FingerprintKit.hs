module Arkham.Asset.Assets.FingerprintKit (
  fingerprintKit,
  FingerprintKit (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigate

newtype FingerprintKit = FingerprintKit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fingerprintKit :: AssetCard FingerprintKit
fingerprintKit = asset FingerprintKit Cards.fingerprintKit

instance HasAbilities FingerprintKit where
  getAbilities (FingerprintKit a) = [investigateAbility a 1 (exhaust a <> assetUseCost a Supply 1) ControlsThis]

instance RunMessage FingerprintKit where
  runMessage msg a@(FingerprintKit attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      investigation <- mkInvestigate sid iid (toAbilitySource attrs 1)
      enabled <- skillTestModifiers sid attrs iid [SkillModifier #intellect 1, DiscoveredClues 1]
      pushAll
        [ enabled
        , toMessage investigation
        ]
      pure a
    _ -> FingerprintKit <$> runMessage msg attrs
