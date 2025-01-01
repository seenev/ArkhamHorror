module Arkham.Asset.Assets.Bonesaw (bonesaw, Bonesaw (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Helpers.SkillTest (getSkillTestTarget, withSkillTest)
import Arkham.Helpers.SkillTest qualified as Msg
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier

newtype Bonesaw = Bonesaw AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

bonesaw :: AssetCard Bonesaw
bonesaw = asset Bonesaw Cards.bonesaw

instance HasAbilities Bonesaw where
  getAbilities (Bonesaw a) =
    [ restrictedAbility a 1 ControlsThis fightAction_
    , skillTestAbility
        $ playerLimit PerGame
        $ controlledAbility
          a
          2
          (exists $ HealableInvestigator (a.ability 2) #damage $ InvestigatorAt YourLocation)
          actionAbility
    ]

instance RunMessage Bonesaw where
  runMessage msg a@(Bonesaw attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      skillTestModifier sid (attrs.ability 1) iid (SkillModifier #combat 2)
      chooseFightEnemy sid iid (attrs.ability 1)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      withSkillTest \sid ->
        chooseOneM iid do
          labeled "Take 1 damage to do +1 damage" do
            assignDamage iid (attrs.ability 1) 1
            skillTestModifier sid (attrs.ability 1) iid (DamageDealt 1)
          labeled "Do not take damage" nothing
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      investigators <- select $ HealableInvestigator (attrs.ability 2) #damage $ colocatedWith iid
      sid <- getRandom
      when (notNull investigators) do
        chooseOne
          iid
          [ targetLabel
            iid'
            [ HealDamage (toTarget iid') (attrs.ability 2) 5
            , Msg.beginSkillTest sid iid (attrs.ability 2) iid' #intellect (Fixed 4)
            ]
          | iid' <- investigators
          ]
      pure a
    FailedThisSkillTest _iid (isAbilitySource attrs 2 -> True) -> do
      getSkillTestTarget >>= \case
        Just (InvestigatorTarget iid') -> push $ SufferTrauma iid' 1 0
        _ -> pure ()
      pure a
    _ -> Bonesaw <$> liftRunMessage msg attrs
