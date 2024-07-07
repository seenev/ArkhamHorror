module Arkham.Asset.Cards.GrislyTotemSeeker3 (
  grislyTotemSeeker3,
  grislyTotemSeeker3Effect,
  GrislyTotemSeeker3 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.Helpers.Card
import Arkham.Matcher
import Arkham.SkillType
import Arkham.Timing qualified as Timing
import Arkham.Window (Window (..))
import Arkham.Window qualified as Window

newtype GrislyTotemSeeker3 = GrislyTotemSeeker3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotemSeeker3 :: AssetCard GrislyTotemSeeker3
grislyTotemSeeker3 = asset GrislyTotemSeeker3 Cards.grislyTotemSeeker3

instance HasAbilities GrislyTotemSeeker3 where
  getAbilities (GrislyTotemSeeker3 a) =
    [ restrictedAbility a 1 ControlsThis
        $ ReactionAbility (CommittedCard Timing.After You AnyCard) (exhaust a)
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

instance RunMessage GrislyTotemSeeker3 where
  runMessage msg a@(GrislyTotemSeeker3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (getCard -> card) _ -> do
      icons <- setFromList @(Set SkillIcon) <$> iconsForCard card
      player <- getPlayer iid
      pushAll
        [ chooseOrRunOne player
            $ [ Label (toSkillLabel icon)
                $ [skillTestModifier (toAbilitySource attrs 1) (toCardId card) (AddSkillIcons [icon])]
              | icon <- setToList icons
              ]
        , createCardEffect Cards.grislyTotemSeeker3 Nothing (toAbilitySource attrs 1) (toTarget iid)
        ]
      pure a
    _ -> GrislyTotemSeeker3 <$> runMessage msg attrs

newtype GrislyTotemSeeker3Effect = GrislyTotemSeeker3Effect EffectAttrs
  deriving anyclass (HasAbilities, HasModifiersFor, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

grislyTotemSeeker3Effect :: EffectArgs -> GrislyTotemSeeker3Effect
grislyTotemSeeker3Effect = cardEffect GrislyTotemSeeker3Effect Cards.grislyTotemSeeker3

instance RunMessage GrislyTotemSeeker3Effect where
  runMessage msg e@(GrislyTotemSeeker3Effect attrs) =
    case msg of
      PassedSkillTest iid _ _ _ _ _ -> do
        let drawing = drawCards iid (effectSource attrs) 1
        pushAll [DisableEffect (toId attrs), drawing]
        pure e
      SkillTestEnds _ _ -> do
        push $ DisableEffect $ toId attrs
        pure e
      _ -> GrislyTotemSeeker3Effect <$> runMessage msg attrs
