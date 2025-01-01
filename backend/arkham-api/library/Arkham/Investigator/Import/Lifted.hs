module Arkham.Investigator.Import.Lifted (
  module X,
  module Arkham.Investigator.Import.Lifted,
)
where

import Arkham.Classes as X
import Arkham.Investigator.Runner as X (
  ChaosTokenFace (ElderSign),
  ChaosTokenModifier (..),
  ChaosTokenValue (..),
  Field (InvestigatorDiscard),
  InvestigatorAttrs (..),
  InvestigatorCard,
  IsInvestigator,
  Stats (..),
  deckL,
  resignedL,
  defeatedL,
  decksL,
  deleteMetaKey,
  insertMetaKey,
  investigator,
  investigatorWith,
  is,
  lookupMetaKeyWithDefault,
  overMetaKey,
  push,
  pushAll,
  pushIfAny,
  pushWhen,
  pushWhenM,
  setMeta,
  setMetaKey,
  slotsL,
  startsWith,
  usedAdditionalActionsL,
  pattern PositiveModifier,
  pattern ZeroModifier,
 )
import Arkham.Message as X (
  Message (..),
  pattern BeginSkillTest,
  pattern ElderSignEffect,
  pattern PassedSkillTestWithToken,
  pattern PassedThisSkillTest,
  pattern PassedThisSkillTestBy,
  pattern UseThisAbility,
 )
import Arkham.Message.Lifted as X
import Arkham.Prelude as X
import Arkham.Question as X
import Arkham.Source as X
import Arkham.Target as X

import Arkham.Calculation

elderSignValue :: GameCalculation -> ChaosTokenValue
elderSignValue = ChaosTokenValue ElderSign . CalculatedModifier
