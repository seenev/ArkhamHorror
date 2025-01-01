module Arkham.Asset.Assets.TimewornBrand5 (timewornBrand5, TimewornBrand5 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Matcher hiding (EnemyDefeated)
import Arkham.Modifier
import Arkham.Trait (Trait (Elite))

newtype TimewornBrand5 = TimewornBrand5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

timewornBrand5 :: AssetCard TimewornBrand5
timewornBrand5 = asset TimewornBrand5 Cards.timewornBrand5

instance HasAbilities TimewornBrand5 where
  getAbilities (TimewornBrand5 a) =
    [ withTooltip
        "{action} If Timeworn Brand is ready: _Fight_. You get +2 {combat} and deal +1 damage for this attack."
        $ controlled a 1 (thisIs a $ asset_ #ready) fightAction_
    , withTooltip
        "{action} Exhaust Timeworn Brand: _Fight_. Add your {willpower} to your skill value for this attack. This attack deals +3 damage. If this attack defeats an _Elite_ enemy, draw 3 cards. (Max once per game.)"
        $ limitedAbility (MaxPer Cards.timewornBrand5 PerGame 1)
        $ restricted a 2 ControlsThis
        $ fightAction (exhaust a)
    ]

instance RunMessage TimewornBrand5 where
  runMessage msg a@(TimewornBrand5 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      skillTestModifiers sid source iid [SkillModifier #combat 2, DamageDealt 1]
      chooseFightEnemy sid iid source
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = attrs.ability 2
      sid <- getRandom
      skillTestModifiers sid source iid [AddSkillValue #willpower, DamageDealt 3]
      chooseFightEnemy sid iid source
      pure a
    EnemyDefeated _ _ (isAbilitySource attrs 2 -> True) traits | Elite `elem` traits -> do
      for_ attrs.controller \iid -> drawCardsIfCan iid (attrs.ability 2) 3
      pure a
    _ -> TimewornBrand5 <$> liftRunMessage msg attrs
