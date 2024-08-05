module Arkham.Asset.Cards.BrandOfCthugha4 (brandOfCthugha4, BrandOfCthugha4 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Id
import Arkham.Prelude

newtype BrandOfCthugha4 = BrandOfCthugha4 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

brandOfCthugha4 :: AssetCard BrandOfCthugha4
brandOfCthugha4 = asset BrandOfCthugha4 Cards.brandOfCthugha4

instance HasAbilities BrandOfCthugha4 where
  getAbilities (BrandOfCthugha4 a) = [restrictedAbility a 1 ControlsThis fightAction_]

instance RunMessage BrandOfCthugha4 where
  runMessage msg a@(BrandOfCthugha4 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      choices <- for [#willpower, #combat] \sType -> do
        chooseFight <- toMessage . withSkillType sType <$> mkChooseFight sid iid (attrs.ability 1)
        pure
          $ SkillLabel
            sType
            [ skillTestModifiers sid (attrs.ability 1) iid [SkillModifier sType 2, NoStandardDamage]
            , chooseFight
            ]
      player <- getPlayer iid
      push $ chooseOne player choices
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      player <- getPlayer iid
      pushWhen (n == 0) $ LoseActions iid (attrs.ability 1) 2
      case attrs.use Charge of
        0 -> pure ()
        1 -> do
          charges <- namedUUID "Charges"
          push $ ResolveAmounts iid [(charges, 1)] (toTarget attrs)
        (min 3 -> x) ->
          pushM
            $ chooseAmounts player "Amount of Charges to Spend" (MaxAmountTarget x) [("Charges", (1, x))] attrs
      pure a
    ResolveAmounts iid (getChoiceAmount "Charges" -> n) (isTarget attrs -> True) -> do
      withSkillTest \sid ->
        pushAll
          [ skillTestModifier sid (attrs.ability 1) iid (DamageDealt n)
          , SpendUses (attrs.ability 1) (toTarget attrs) Charge n
          ]
      pure a
    _ -> BrandOfCthugha4 <$> runMessage msg attrs
