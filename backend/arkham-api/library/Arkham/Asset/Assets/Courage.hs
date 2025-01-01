module Arkham.Asset.Assets.Courage (
  courage,
  Courage (..),
) where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner

newtype Courage = Courage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

courage :: AssetCard Courage
courage = assetWith Courage Cards.courage (sanityL ?~ 2)

instance RunMessage Courage where
  runMessage msg (Courage attrs) = Courage <$> runMessage msg attrs
