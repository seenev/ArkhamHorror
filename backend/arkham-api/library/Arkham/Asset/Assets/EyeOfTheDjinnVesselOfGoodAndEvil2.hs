module Arkham.Asset.Assets.EyeOfTheDjinnVesselOfGoodAndEvil2 (
  eyeOfTheDjinnVesselOfGoodAndEvil2,
  eyeOfTheDjinnVesselOfGoodAndEvil2Effect,
  EyeOfTheDjinnVesselOfGoodAndEvil2 (..),
)
where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Effect.Import
import Arkham.Helpers.SkillTest (getSkillTestId, getSkillTestSkillTypes, withSkillTest)
import Arkham.Matcher hiding (DuringTurn, RevealChaosToken, SkillTestEnded)
import Arkham.Modifier

newtype EyeOfTheDjinnVesselOfGoodAndEvil2 = EyeOfTheDjinnVesselOfGoodAndEvil2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfTheDjinnVesselOfGoodAndEvil2 :: AssetCard EyeOfTheDjinnVesselOfGoodAndEvil2
eyeOfTheDjinnVesselOfGoodAndEvil2 = asset EyeOfTheDjinnVesselOfGoodAndEvil2 Cards.eyeOfTheDjinnVesselOfGoodAndEvil2

instance HasAbilities EyeOfTheDjinnVesselOfGoodAndEvil2 where
  getAbilities (EyeOfTheDjinnVesselOfGoodAndEvil2 x) =
    [ controlledAbility x 1 (DuringTurn You)
        $ ReactionAbility (InitiatedSkillTest #when You #any #any #any) (exhaust x)
    ]

instance RunMessage EyeOfTheDjinnVesselOfGoodAndEvil2 where
  runMessage msg a@(EyeOfTheDjinnVesselOfGoodAndEvil2 attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sTypes <- getSkillTestSkillTypes
      withSkillTest \sid -> do
        skillTestModifiers sid (attrs.ability 1) iid [BaseSkillOf sType 5 | sType <- sTypes]
        createCardEffect
          Cards.eyeOfTheDjinnVesselOfGoodAndEvil2
          (effectMetaTarget sid)
          (attrs.ability 1)
          attrs
        createCardEffect
          Cards.eyeOfTheDjinnVesselOfGoodAndEvil2
          (effectMetaTarget sid)
          (attrs.ability 1)
          iid
      pure a
    _ -> EyeOfTheDjinnVesselOfGoodAndEvil2 <$> liftRunMessage msg attrs

newtype EyeOfTheDjinnVesselOfGoodAndEvil2Effect = EyeOfTheDjinnVesselOfGoodAndEvil2Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eyeOfTheDjinnVesselOfGoodAndEvil2Effect :: EffectArgs -> EyeOfTheDjinnVesselOfGoodAndEvil2Effect
eyeOfTheDjinnVesselOfGoodAndEvil2Effect =
  cardEffectWith
    EyeOfTheDjinnVesselOfGoodAndEvil2Effect
    Cards.eyeOfTheDjinnVesselOfGoodAndEvil2
    (setEffectMeta False)

instance RunMessage EyeOfTheDjinnVesselOfGoodAndEvil2Effect where
  runMessage msg e@(EyeOfTheDjinnVesselOfGoodAndEvil2Effect attrs) = runQueueT $ case msg of
    RevealChaosToken _ _ token -> do
      void $ runMaybeT do
        SkillTestTarget sid <- hoistMaybe attrs.metaTarget
        current <- MaybeT getSkillTestId
        guard $ sid == current
        lift $ case attrs.target of
          AssetTarget aid -> when (token.face == #bless) do
            whenM (aid <!=~> AssetReady) do
              disable attrs
              readyThis aid
          InvestigatorTarget iid -> when (token.face == #curse) do
            send "Gained 1 action from Eye of the Djinn"
            disable attrs
            gainActions iid attrs.source 1
          _ -> error "Invalid target"
      pure e
    SkillTestEnded sid | maybe False (isTarget sid) attrs.metaTarget -> disableReturn e
    _ -> EyeOfTheDjinnVesselOfGoodAndEvil2Effect <$> liftRunMessage msg attrs
