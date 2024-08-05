module Arkham.SkillType where

import Arkham.Classes.GameLogger
import Arkham.Prelude
import GHC.OverloadedLabels

newtype CommittedSkillIcon = CommittedSkillIcon {unCommittedSkillIcon :: SkillIcon}
  deriving newtype (Show, Eq, Generic, Ord, ToJSON, FromJSON, Hashable)

data SkillType
  = SkillWillpower
  | SkillIntellect
  | SkillCombat
  | SkillAgility
  deriving stock (Show, Eq, Bounded, Enum, Generic, Ord, Data)
  deriving anyclass (ToJSON, FromJSON, Hashable)

instance ToGameLoggerFormat SkillType where
  format = \case
    SkillWillpower -> "{willpower}"
    SkillIntellect -> "{intellect}"
    SkillCombat -> "{combat}"
    SkillAgility -> "{agility}"

allSkills :: [SkillType]
allSkills = [minBound ..]

labeledSkills :: [(Text, SkillType)]
labeledSkills =
  [("Willpower", #willpower), ("Intellect", #intellect), ("Combat", #combat), ("Agility", #agility)]

instance IsLabel "willpower" SkillType where
  fromLabel = SkillWillpower

instance IsLabel "intellect" SkillType where
  fromLabel = SkillIntellect

instance IsLabel "combat" SkillType where
  fromLabel = SkillCombat

instance IsLabel "agility" SkillType where
  fromLabel = SkillAgility

data SkillIcon = SkillIcon SkillType | WildIcon | WildMinusIcon
  deriving stock (Show, Eq, Generic, Ord, Data)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, Hashable)

instance IsLabel "willpower" SkillIcon where
  fromLabel = SkillIcon SkillWillpower

instance IsLabel "intellect" SkillIcon where
  fromLabel = SkillIcon SkillIntellect

instance IsLabel "combat" SkillIcon where
  fromLabel = SkillIcon SkillCombat

instance IsLabel "agility" SkillIcon where
  fromLabel = SkillIcon SkillAgility

instance IsLabel "wild" SkillIcon where
  fromLabel = WildIcon

instance IsLabel "wildMinus" SkillIcon where
  fromLabel = WildMinusIcon
