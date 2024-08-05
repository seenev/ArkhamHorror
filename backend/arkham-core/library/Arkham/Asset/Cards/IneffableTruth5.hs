module Arkham.Asset.Cards.IneffableTruth5 (ineffableTruth5, IneffableTruth5 (..), ineffableTruth5Effect) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Aspect
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.ChaosToken
import Arkham.DamageEffect
import Arkham.Effect.Runner
import Arkham.Evade
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype IneffableTruth5 = IneffableTruth5 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth5 :: AssetCard IneffableTruth5
ineffableTruth5 = asset IneffableTruth5 Cards.ineffableTruth5

instance HasAbilities IneffableTruth5 where
  getAbilities (IneffableTruth5 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ActionAbility
          [Action.Evade]
          (Costs [ActionCost 1, UseCost (AssetWithId $ toId a) Charge 1])
    ]

instance RunMessage IneffableTruth5 where
  runMessage msg a@(IneffableTruth5 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = attrs.ability 1
      sid <- getRandom
      chooseEvade <-
        leftOr <$> aspect iid source (#willpower `InsteadOf` #agility) (mkChooseEvade sid iid source)
      pushAll
        $ [ createCardEffect Cards.ineffableTruth5 (effectInt 1) source sid
          , createCardEffect Cards.ineffableTruth5 (effectInt 2) source sid
          , skillTestModifier sid source iid (SkillModifier #willpower 3)
          ]
        <> chooseEvade
      pure a
    _ -> IneffableTruth5 <$> runMessage msg attrs

newtype IneffableTruth5Effect = IneffableTruth5Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ineffableTruth5Effect :: EffectArgs -> IneffableTruth5Effect
ineffableTruth5Effect = cardEffect IneffableTruth5Effect Cards.ineffableTruth5

instance RunMessage IneffableTruth5Effect where
  runMessage msg e@(IneffableTruth5Effect attrs) = case msg of
    RevealChaosToken _ iid token | InvestigatorTarget iid == attrs.target -> do
      whenJustM getSkillTest \st -> do
        let triggers =
              token.face
                `elem` [ElderSign, PlusOne, Zero]
                && iid
                == st.investigator
                && isTarget st attrs.target
                && attrs.metaInt
                == Just 1
        when triggers do
          pushAll
            [ If
                (Window.RevealChaosTokenEffect iid token attrs.id)
                [LoseResources iid attrs.source 2]
            , disable attrs
            ]
      pure e
    SkillTestEnds sid _ _ | isTarget sid attrs.target -> do
      push $ disable attrs
      pure e
    PassedSkillTest iid (Just Action.Evade) _ (SkillTestInitiatorTarget (EnemyTarget eid)) _ _ -> do
      withSkillTest \sid -> do
        when (isTarget sid attrs.target && attrs.metaInt == Just 2) do
          pushAll
            [ EnemyDamage eid $ nonAttack iid 2
            , disable attrs
            ]
      pure e
    _ -> IneffableTruth5Effect <$> runMessage msg attrs
