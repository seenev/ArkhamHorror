module Arkham.Location.Cards.Rivertown_293 (
  rivertown_293,
  Rivertown_293 (..),
)
where

import Arkham.Prelude

import Arkham.GameValue
import Arkham.Location.BreachStatus
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher

newtype Rivertown_293 = Rivertown_293 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rivertown_293 :: LocationCard Rivertown_293
rivertown_293 = location Rivertown_293 Cards.rivertown_293 4 (Static 0)

instance HasAbilities Rivertown_293 where
  getAbilities (Rivertown_293 attrs) =
    withRevealedAbilities
      attrs
      [skillTestAbility $ restrictedAbility attrs 1 Here $ ActionAbility [] $ ActionCost 1]

instance RunMessage Rivertown_293 where
  runMessage msg l@(Rivertown_293 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure l
    PassedSkillTest _iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ n -> do
      let n' = min (maybe 0 countBreaches $ locationBreaches attrs) n
      act <- selectJust AnyAct
      pushAll [RemoveBreaches (toTarget attrs) n', PlaceBreaches (toTarget act) n']
      pure l
    _ -> Rivertown_293 <$> runMessage msg attrs
