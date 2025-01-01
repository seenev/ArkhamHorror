module Arkham.Asset.Assets.GrislyTotem (grislyTotem, GrislyTotem (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Import.Lifted
import Arkham.Card
import Arkham.Helpers.Card
import Arkham.Helpers.SkillTest (withSkillTest)
import Arkham.Matcher
import Arkham.Message.Lifted.Choose
import Arkham.Modifier
import Arkham.SkillType
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype GrislyTotem = GrislyTotem AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotem :: AssetCard GrislyTotem
grislyTotem = asset GrislyTotem Cards.grislyTotem

instance HasAbilities GrislyTotem where
  getAbilities (GrislyTotem a) =
    [ restricted a 1 ControlsThis $ ReactionAbility (CommittedCard #after You AnyCard) (exhaust a)
    ]

getCard :: [Window] -> Card
getCard [] = error "missing card"
getCard ((windowType -> Window.CommittedCard _ c) : _) = c
getCard (_ : ws) = getCard ws

toSkillLabel :: SkillIcon -> Text
toSkillLabel WildMinusIcon = "Choose Minus {wild}"
toSkillLabel WildIcon = "Choose {wild}"
toSkillLabel (SkillIcon sType) = case sType of
  SkillWillpower -> "Choose {willpower}"
  SkillIntellect -> "Choose {intellect}"
  SkillCombat -> "Choose {combat}"
  SkillAgility -> "Choose {agility}"

instance RunMessage GrislyTotem where
  runMessage msg a@(GrislyTotem attrs) = runQueueT $ case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCard -> card) _ -> do
      icons <- setFromList @(Set SkillIcon) <$> iconsForCard card
      withSkillTest \sid -> do
        chooseOrRunOneM iid do
          for_ (setToList icons) \icon -> do
            labeled (toSkillLabel icon) do
              skillTestModifier sid attrs card (AddSkillIcons [icon])
      pure a
    _ -> GrislyTotem <$> liftRunMessage msg attrs
