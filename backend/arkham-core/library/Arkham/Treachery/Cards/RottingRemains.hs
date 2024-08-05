module Arkham.Treachery.Cards.RottingRemains where

import Arkham.Classes
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype RottingRemains = RottingRemains TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rottingRemains :: TreacheryCard RottingRemains
rottingRemains = treachery RottingRemains Cards.rottingRemains

instance RunMessage RottingRemains where
  runMessage msg t@(RottingRemains attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ assignHorror iid attrs n
      pure t
    _ -> RottingRemains <$> runMessage msg attrs
