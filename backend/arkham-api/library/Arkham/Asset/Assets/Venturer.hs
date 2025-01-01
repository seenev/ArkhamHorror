module Arkham.Asset.Assets.Venturer (
  venturer,
  Venturer (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher

newtype Venturer = Venturer AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

venturer :: AssetCard Venturer
venturer = ally Venturer Cards.venturer (2, 2)

instance HasAbilities Venturer where
  getAbilities (Venturer a) =
    [ controlledAbility
        a
        1
        ( exists
            $ AssetControlledBy (affectsOthers $ InvestigatorAt YourLocation)
            <> AssetOneOf [AssetWithUseType Supply, AssetWithUseType Ammo]
            <> AssetNotAtUseLimit
            <> NotAsset (AssetWithId $ toId a)
        )
        $ FastAbility
        $ ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Supply 1
    ]

instance RunMessage Venturer where
  runMessage msg a@(Venturer attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      supplyAssets <-
        select
          $ AssetWithUseType Supply
          <> AssetControlledBy (affectsOthers $ colocatedWith iid)
          <> NotAsset (AssetWithId $ toId attrs)
      ammoAssets <-
        select
          $ AssetWithUseType Ammo
          <> AssetControlledBy
            (affectsOthers $ colocatedWith iid)
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ targetLabel aid [AddUses (attrs.ability 1) aid Supply 1]
          | aid <- supplyAssets
          ]
        <> [ targetLabel aid [AddUses (attrs.ability 1) aid Ammo 1]
           | aid <- ammoAssets
           ]
      pure a
    _ -> Venturer <$> runMessage msg attrs
