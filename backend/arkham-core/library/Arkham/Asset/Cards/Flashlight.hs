module Arkham.Asset.Cards.Flashlight (
  Flashlight (..),
  flashlight,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Helpers.Investigator
import Arkham.Investigate

newtype Flashlight = Flashlight AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

flashlight :: AssetCard Flashlight
flashlight = asset Flashlight Cards.flashlight

instance HasAbilities Flashlight where
  getAbilities (Flashlight x) = [investigateAbility x 1 (assetUseCost x Supply 1) ControlsThis]

instance RunMessage Flashlight where
  runMessage msg a@(Flashlight attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      lid <- getJustLocation iid
      sid <- getRandom
      investigation <- mkInvestigate sid iid (toAbilitySource attrs 1)
      pushAll
        [ skillTestModifier sid attrs lid (ShroudModifier (-2))
        , toMessage investigation
        ]
      pure a
    _ -> Flashlight <$> runMessage msg attrs
