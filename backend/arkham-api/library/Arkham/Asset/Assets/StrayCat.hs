module Arkham.Asset.Assets.StrayCat (
  StrayCat (..),
  strayCat,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher hiding (EnemyEvaded)

newtype StrayCat = StrayCat AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

strayCat :: AssetCard StrayCat
strayCat = ally StrayCat Cards.strayCat (1, 0)

instance HasAbilities StrayCat where
  getAbilities (StrayCat a) =
    [ controlledAbility
        a
        1
        (exists (EnemyAt YourLocation <> NonEliteEnemy <> EnemyWithoutModifier CannotBeEvaded))
        $ FastAbility
        $ discardCost a
    ]

instance RunMessage StrayCat where
  runMessage msg a@(StrayCat attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      enemies <- select $ enemyAtLocationWith iid <> NonEliteEnemy <> EnemyWithoutModifier CannotBeEvaded
      player <- getPlayer iid
      push $ chooseOne player $ targetLabels enemies (only . EnemyEvaded iid)
      pure a
    _ -> StrayCat <$> runMessage msg attrs
