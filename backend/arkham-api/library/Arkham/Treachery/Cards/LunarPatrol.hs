module Arkham.Treachery.Cards.LunarPatrol (lunarPatrol, LunarPatrol (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenarios.DarkSideOfTheMoon.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype LunarPatrol = LunarPatrol TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lunarPatrol :: TreacheryCard LunarPatrol
lunarPatrol = treachery LunarPatrol Cards.lunarPatrol

instance HasAbilities LunarPatrol where
  getAbilities (LunarPatrol a) =
    [ skillTestAbility $ restrictedAbility a 1 OnSameLocation actionAbility
    , mkAbility a 2 $ forced $ Leaves #when You $ locationWithTreachery a
    ]

instance RunMessage LunarPatrol where
  runMessage msg t@(LunarPatrol attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      mLocation <- getMaybeLocation iid
      for_ mLocation $ attachTreachery attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 3)
      pure t
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      raiseAlarmLevel attrs iid
      pure t
    _ -> LunarPatrol <$> liftRunMessage msg attrs
