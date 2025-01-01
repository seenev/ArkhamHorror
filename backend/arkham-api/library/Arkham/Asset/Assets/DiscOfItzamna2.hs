module Arkham.Asset.Assets.DiscOfItzamna2 where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype DiscOfItzamna2 = DiscOfItzamna2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

discOfItzamna2 :: AssetCard DiscOfItzamna2
discOfItzamna2 = asset DiscOfItzamna2 Cards.discOfItzamna2

instance HasAbilities DiscOfItzamna2 where
  getAbilities (DiscOfItzamna2 a) =
    [restrictedAbility a 1 ControlsThis $ freeReaction (EnemySpawns #when YourLocation NonEliteEnemy)]

instance RunMessage DiscOfItzamna2 where
  runMessage msg a@(DiscOfItzamna2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      -- this does not cancel so we must remove manually
      menemySpawnMessage <- fromQueue $ find ((== Just EnemySpawnMessage) . messageType)
      case menemySpawnMessage of
        Just msg'@(EnemySpawn _ _ enemyId) ->
          replaceMessage
            msg'
            [ toDiscardBy iid (toAbilitySource attrs 1) attrs
            , toDiscardBy iid (toAbilitySource attrs 1) enemyId
            ]
        _ -> pure ()
      pure a
    _ -> DiscOfItzamna2 <$> runMessage msg attrs
