module Arkham.Asset.Cards.FeedTheMind3 (feedTheMind3, FeedTheMind3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection

newtype FeedTheMind3 = FeedTheMind3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

feedTheMind3 :: AssetCard FeedTheMind3
feedTheMind3 = asset FeedTheMind3 Cards.feedTheMind3

instance HasAbilities FeedTheMind3 where
  getAbilities (FeedTheMind3 a) =
    [ skillTestAbility
        $ restrictedAbility a 1 ControlsThis
        $ actionAbilityWithCost
        $ exhaust a
        <> assetUseCost a Secret 1
    ]

instance RunMessage FeedTheMind3 where
  runMessage msg a@(FeedTheMind3 attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      sid <- getRandom
      push $ beginSkillTest sid iid (attrs.ability 1) iid #intellect (Fixed 0)
      pure a
    PassedThisSkillTestBy iid (isAbilitySource attrs 1 -> True) n -> do
      let drawing = drawCards iid (attrs.ability 1) n
      pushAll
        [ drawing
        , HandleTargetChoice iid (attrs.ability 1) (toTarget attrs)
        ]
      pure a
    HandleTargetChoice iid (isAbilitySource attrs 1 -> True) _ -> do
      handSize <- field InvestigatorHandSize iid
      handCount <- fieldMap InvestigatorHand length iid
      let n = handCount - handSize
      pushWhen (n > 0) $ assignHorror iid (toAbilitySource attrs 1) n
      pure a
    _ -> FeedTheMind3 <$> runMessage msg attrs
