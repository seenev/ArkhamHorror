module Arkham.Asset.Assets.FirstAid (FirstAid (..), firstAid) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner hiding (allInvestigators)
import Arkham.Matcher
import Arkham.Prelude

newtype FirstAid = FirstAid AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstAid :: AssetCard FirstAid
firstAid = assetWith FirstAid Cards.firstAid discardWhenNoUses

instance HasAbilities FirstAid where
  getAbilities (FirstAid x) = [controlledAbility x 1 criteria $ actionAbilityWithCost $ assetUseCost x Supply 1]
   where
    criteria = exists $ oneOf $ map healable [#horror, #damage]
    healable hType = HealableInvestigator (toAbilitySource x 1) hType $ InvestigatorAt YourLocation

instance RunMessage FirstAid where
  runMessage msg a@(FirstAid attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      withHorror <- select $ HealableInvestigator source #horror $ colocatedWith iid
      withDamage <- select $ HealableInvestigator source #damage $ colocatedWith iid
      player <- getPlayer iid
      choices <- for (toList $ withHorror <> withDamage) \i -> do
        pure
          $ targetLabel i
          . only
          $ chooseOrRunOne player
          $ [DamageLabel i [HealDamage (toTarget i) source 1] | i `elem` withDamage]
          <> [HorrorLabel i [HealHorror (toTarget i) source 1] | i `elem` withHorror]
      pushIfAny choices $ chooseOne player choices
      pure a
    _ -> FirstAid <$> runMessage msg attrs
