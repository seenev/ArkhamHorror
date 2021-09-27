module Arkham.Types.Skill.Cards.DesperateSearch
  ( desperateSearch
  , DesperateSearch(..)
  ) where

import Arkham.Prelude

import Arkham.Skill.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Skill.Attrs
import Arkham.Types.Skill.Runner

newtype DesperateSearch = DesperateSearch SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

desperateSearch :: SkillCard DesperateSearch
desperateSearch = skill DesperateSearch Cards.desperateSearch

instance SkillRunner env => RunMessage env DesperateSearch where
  runMessage msg (DesperateSearch attrs) =
    DesperateSearch <$> runMessage msg attrs
