module Arkham.Asset.Cards.Kerosene1 (
  kerosene1,
  Kerosene1 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher

newtype Kerosene1 = Kerosene1 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

kerosene1 :: AssetCard Kerosene1
kerosene1 = assetWith Kerosene1 Cards.kerosene1 (whenNoUsesL ?~ DiscardWhenNoUses)

instance HasAbilities Kerosene1 where
  getAbilities (Kerosene1 a) =
    [ restrictedAbility
        a
        1
        ( ControlsThis
            <> LocationExists
              (LocationOfThis <> LocationWithDefeatedEnemyThisRound)
            <> AnyCriterion
              [ InvestigatorExists
                  ( HealableInvestigator (toSource a) HorrorType
                      $ InvestigatorAt YourLocation
                  )
              , AssetExists
                  ( HealableAsset (toSource a) HorrorType
                      $ AssetAt YourLocation
                      <> AssetControlledBy (affectsOthers Anyone)
                  )
              ]
        )
        $ ActionAbility []
        $ ActionCost 1
        <> ExhaustCost (toTarget a)
        <> UseCost (AssetWithId $ toId a) Supply 1
    ]

instance RunMessage Kerosene1 where
  runMessage msg a@(Kerosene1 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      totalInvestigatorHorror <-
        getSum
          <$> selectAgg
            Sum
            InvestigatorHorror
            (HealableInvestigator (attrs.ability 1) #horror $ colocatedWith iid)
      totalAssetHorror <-
        getSum
          <$> selectAgg
            Sum
            AssetHorror
            ( HealableAsset (attrs.ability 1) #horror
                $ AssetAt (locationWithInvestigator iid)
                <> AssetControlledBy (affectsOthers Anyone)
            )

      let maxHorror = min 2 (totalInvestigatorHorror + totalAssetHorror)

      player <- getPlayer iid
      pushM
        $ chooseAmounts
          player
          "Choose amount of horror to heal"
          (MaxAmountTarget maxHorror)
          [("Horror", (0, maxHorror))]
          (toTarget attrs)
      pure a
    ResolveAmounts iid (getChoiceAmount "Horror" -> n) (isTarget attrs -> True) ->
      do
        pushAll
          $ replicate n
          $ UseCardAbilityChoice
            iid
            (toSource attrs)
            1
            NoAbilityMetadata
            []
            NoPayment
        pure a
    UseCardAbilityChoice iid (isSource attrs -> True) 1 _ _ _ -> do
      investigators <- selectTargets $ HealableInvestigator (attrs.ability 1) #horror $ colocatedWith iid

      assets <-
        selectTargets
          $ HealableAsset (attrs.ability 1) HorrorType
          $ AssetAt (locationWithInvestigator iid)
          <> AssetControlledBy (affectsOthers Anyone)

      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ TargetLabel target [HealHorror target (toSource attrs) 1]
          | target <- assets <> investigators
          ]
      pure a
    _ -> Kerosene1 <$> runMessage msg attrs
