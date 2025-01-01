module Arkham.Asset.Assets.FamiliarSpirit (
  familiarSpirit,
  FamiliarSpirit (..),
)
where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Trait

newtype FamiliarSpirit = FamiliarSpirit AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

familiarSpirit :: AssetCard FamiliarSpirit
familiarSpirit = ally FamiliarSpirit Cards.familiarSpirit (1, 1)

slot :: AssetAttrs -> Slot
slot attrs = TraitRestrictedSlot (toSource attrs) Spell []

instance RunMessage FamiliarSpirit where
  runMessage msg (FamiliarSpirit attrs) = case msg of
    CardIsEnteringPlay iid card | toCardId card == toCardId attrs -> do
      push $ AddSlot iid ArcaneSlot (slot attrs)
      FamiliarSpirit <$> runMessage msg attrs
    _ -> FamiliarSpirit <$> runMessage msg attrs
