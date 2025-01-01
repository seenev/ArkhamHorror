module Arkham.Location.Cards.PeaksOfThok (peaksOfThok, PeaksOfThok (..)) where

import Arkham.Ability
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Matcher
import Arkham.Story.Cards qualified as Story

newtype PeaksOfThok = PeaksOfThok LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

peaksOfThok :: LocationCard PeaksOfThok
peaksOfThok = locationWith PeaksOfThok Cards.peaksOfThok 3 (Static 0) (canBeFlippedL .~ True)

instance HasAbilities PeaksOfThok where
  getAbilities (PeaksOfThok attrs) =
    extendRevealed
      attrs
      [ skillTestAbility $ restrictedAbility attrs 1 Here actionAbility
      , skillTestAbility $ mkAbility attrs 2 $ forced $ Leaves #after You (be attrs)
      ]

instance RunMessage PeaksOfThok where
  runMessage msg l@(PeaksOfThok attrs) = runQueueT $ case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 1) iid #agility (Fixed 5)
      pure l
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      when (locationCanBeFlipped attrs) $ flipOver iid attrs
      pure l
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      sid <- getRandom
      beginSkillTest sid iid (attrs.ability 2) iid #agility (Fixed 2)
      pure l
    FailedThisSkillTest iid (isAbilitySource attrs 2 -> True) -> do
      assignDamage iid (attrs.ability 2) 1
      pure l
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.inhabitantsOfTheVale
      pure . PeaksOfThok $ attrs & canBeFlippedL .~ False
    _ -> PeaksOfThok <$> liftRunMessage msg attrs
