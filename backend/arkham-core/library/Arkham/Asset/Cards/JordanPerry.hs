module Arkham.Asset.Cards.JordanPerry (jordanPerry, JordanPerry (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype JordanPerry = JordanPerry AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

jordanPerry :: AssetCard JordanPerry
jordanPerry = asset JordanPerry Cards.jordanPerry

instance HasAbilities JordanPerry where
  getAbilities (JordanPerry a) =
    [ skillTestAbility
        $ restrictedAbility
          a
          1
          (OnSameLocation <> youExist (InvestigatorWithResources $ atLeast 10))
          parleyAction_
    , groupLimit PerGame
        $ restrictedAbility a 2 (not_ $ exists Story.sickeningReality_66)
        $ forced
        $ LastClueRemovedFromAsset #when (be a)
    ]

instance RunMessage JordanPerry where
  runMessage msg a@(JordanPerry attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ parley sid iid (attrs.ability 1) attrs #intellect (Fixed 2)
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      modifiers <- getModifiers iid
      when (assetClues attrs > 0 && CannotTakeControlOfClues `notElem` modifiers) do
        pushAll [RemoveClues (attrs.ability 1) (toTarget attrs) 1, GainClues iid (attrs.ability 1) 1]
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      lagneauPerdu <- genCard Story.lagneauPerdu
      push $ ReadStory iid lagneauPerdu ResolveIt (Just $ toTarget attrs)
      pure a
    _ -> JordanPerry <$> runMessage msg attrs
