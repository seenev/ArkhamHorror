module Arkham.Act.Cards.DisruptingTheRitual where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Matcher

newtype DisruptingTheRitual = DisruptingTheRitual ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

disruptingTheRitual :: ActCard DisruptingTheRitual
disruptingTheRitual =
  act (3, A) DisruptingTheRitual Cards.disruptingTheRitual Nothing

instance HasAbilities DisruptingTheRitual where
  getAbilities (DisruptingTheRitual a)
    | onSide A a =
        [ skillTestAbility $ mkAbility a 1 $ ActionAbility [] $ Costs [ActionCost 1, ClueCost (Static 1)]
        , restrictedAbility a 2 (CluesOnThis $ AtLeast $ PerPlayer 2) $ Objective $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage DisruptingTheRitual where
  runMessage msg a@(DisruptingTheRitual attrs@ActAttrs {..}) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOne
          player
          [ SkillLabel #willpower [beginSkillTest sid iid (attrs.ability 1) attrs #willpower (Fixed 3)]
          , SkillLabel #agility [beginSkillTest sid iid (attrs.ability 1) attrs #agility (Fixed 3)]
          ]
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId a) source AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == actId && onSide B attrs -> do
      push R1
      pure a
    PassedSkillTest _ _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ -> do
      push $ PlaceClues (toAbilitySource attrs 1) (toTarget attrs) 1
      pure a
    _ -> DisruptingTheRitual <$> runMessage msg attrs
