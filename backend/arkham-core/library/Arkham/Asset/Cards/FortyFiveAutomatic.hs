module Arkham.Asset.Cards.FortyFiveAutomatic (FortyFiveAutomatic (..), fortyFiveAutomatic) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Prelude

newtype FortyFiveAutomatic = FortyFiveAutomatic AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fortyFiveAutomatic :: AssetCard FortyFiveAutomatic
fortyFiveAutomatic = asset FortyFiveAutomatic Cards.fortyFiveAutomatic

instance HasAbilities FortyFiveAutomatic where
  getAbilities (FortyFiveAutomatic a) = [restrictedAbility a 1 ControlsThis $ fightAction $ assetUseCost a Ammo 1]

instance RunMessage FortyFiveAutomatic where
  runMessage msg a@(FortyFiveAutomatic attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid (attrs.ability 1)
      pushAll
        [ skillTestModifiers sid (attrs.ability 1) iid [DamageDealt 1, SkillModifier #combat 1]
        , chooseFight
        ]
      pure a
    _ -> FortyFiveAutomatic <$> runMessage msg attrs
