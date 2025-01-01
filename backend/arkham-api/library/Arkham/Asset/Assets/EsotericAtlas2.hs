module Arkham.Asset.Assets.EsotericAtlas2 (
  esotericAtlas2,
  EsotericAtlas2 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Movement

newtype EsotericAtlas2 = EsotericAtlas2 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

esotericAtlas2 :: AssetCard EsotericAtlas2
esotericAtlas2 = asset EsotericAtlas2 Cards.esotericAtlas2

instance HasAbilities EsotericAtlas2 where
  getAbilities (EsotericAtlas2 a) =
    [ controlledAbility
        a
        1
        (CanMoveTo $ oneOf [LocationWithDistanceFrom n YourLocation RevealedLocation | n <- [1 .. 3]])
        $ actionAbilityWithCost (assetUseCost a Secret 1)
    ]

instance RunMessage EsotericAtlas2 where
  runMessage msg a@(EsotericAtlas2 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      locations <-
        getCanMoveToMatchingLocations iid (attrs.ability 1)
          $ oneOf [LocationWithDistanceFrom n (locationWithInvestigator iid) RevealedLocation | n <- [1 .. 3]]
      player <- getPlayer iid
      push
        $ chooseOne
          player
          [ targetLabel location [Move $ move (toSource attrs) iid location]
          | location <- locations
          ]
      pure a
    _ -> EsotericAtlas2 <$> runMessage msg attrs
