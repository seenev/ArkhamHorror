module Arkham.Asset.Assets.FingerprintKit4 (
  fingerprintKit4,
  FingerprintKit4 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigate

newtype FingerprintKit4 = FingerprintKit4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fingerprintKit4 :: AssetCard FingerprintKit4
fingerprintKit4 = asset FingerprintKit4 Cards.fingerprintKit4

instance HasAbilities FingerprintKit4 where
  getAbilities (FingerprintKit4 a) = [investigateAbility a 1 (exhaust a <> assetUseCost a Supply 1) ControlsThis]

instance RunMessage FingerprintKit4 where
  runMessage msg a@(FingerprintKit4 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      investigation <- mkInvestigate sid iid (toAbilitySource attrs 1)
      enabled <- skillTestModifiers sid attrs iid [SkillModifier #intellect 2, DiscoveredClues 2]
      pushAll
        [ enabled
        , toMessage investigation
        ]
      pure a
    _ -> FingerprintKit4 <$> runMessage msg attrs
