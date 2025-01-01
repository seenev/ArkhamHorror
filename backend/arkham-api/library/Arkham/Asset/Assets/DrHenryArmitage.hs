module Arkham.Asset.Assets.DrHenryArmitage (DrHenryArmitage (..), drHenryArmitage) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Prelude

newtype DrHenryArmitage = DrHenryArmitage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

drHenryArmitage :: AssetCard DrHenryArmitage
drHenryArmitage = ally DrHenryArmitage Cards.drHenryArmitage (2, 2)

instance HasAbilities DrHenryArmitage where
  getAbilities (DrHenryArmitage a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (DrawCard #after You (basic DiscardableCard) (DeckOf You))
        $ Costs [DiscardDrawnCardCost, exhaust a]
    ]

instance RunMessage DrHenryArmitage where
  runMessage msg a@(DrHenryArmitage attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ takeResources iid (attrs.ability 1) 3
      pure a
    _ -> DrHenryArmitage <$> runMessage msg attrs
