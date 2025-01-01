module Arkham.Asset.Assets.FortyFiveThompson (fortyFiveThompson, FortyFiveThompson (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype FortyFiveThompson = FortyFiveThompson AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveThompson :: AssetCard FortyFiveThompson
fortyFiveThompson = asset FortyFiveThompson Cards.fortyFiveThompson

instance HasAbilities FortyFiveThompson where
  getAbilities (FortyFiveThompson a) =
    [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage FortyFiveThompson where
  runMessage msg a@(FortyFiveThompson attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      enabled <- skillTestModifiers sid source iid [DamageDealt 1, SkillModifier #combat 2]
      pushAll [enabled, chooseFight]
      pure a
    _ -> FortyFiveThompson <$> runMessage msg attrs
