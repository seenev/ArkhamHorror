module Arkham.Skill.Cards.DoubleOrNothing (
  doubleOrNothing,
  DoubleOrNothing (..),
) where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Game.Helpers
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype DoubleOrNothing = DoubleOrNothing SkillAttrs
  deriving anyclass (IsSkill, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

doubleOrNothing :: SkillCard DoubleOrNothing
doubleOrNothing = skill DoubleOrNothing Cards.doubleOrNothing

instance HasModifiersFor DoubleOrNothing where
  getModifiersFor (SkillTestTarget _) (DoubleOrNothing attrs) =
    pure $ toModifiers attrs [DoubleDifficulty, DoubleSuccess]
  getModifiersFor _ _ = pure []

instance RunMessage DoubleOrNothing where
  runMessage msg (DoubleOrNothing attrs) =
    DoubleOrNothing <$> runMessage msg attrs
