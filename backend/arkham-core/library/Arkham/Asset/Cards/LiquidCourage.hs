module Arkham.Asset.Cards.LiquidCourage (liquidCourage, LiquidCourage (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Damage
import Arkham.Matcher
import Arkham.Prelude

newtype LiquidCourage = LiquidCourage AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

liquidCourage :: AssetCard LiquidCourage
liquidCourage = asset LiquidCourage Cards.liquidCourage

instance HasAbilities LiquidCourage where
  getAbilities (LiquidCourage x) =
    [ controlledAbility
        x
        1
        (exists (HealableInvestigator (toSource x) #horror $ InvestigatorAt YourLocation))
        $ actionAbilityWithCost (assetUseCost x Supply 1)
    ]

instance RunMessage LiquidCourage where
  runMessage msg a@(LiquidCourage attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      iids <- select $ HealableInvestigator (toSource attrs) HorrorType $ colocatedWith iid
      player <- getPlayer iid
      sid <- getRandom
      pushIfAny iids
        $ chooseOrRunOne
          player
          [ targetLabel
            iid'
            [ HealHorrorWithAdditional (toTarget iid') (toSource attrs) 1
            , beginSkillTest sid iid' (attrs.ability 1) iid' #willpower (Fixed 2)
            ]
          | iid' <- iids
          ]
      pure a
    PassedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      push $ AdditionalHealHorror (toTarget iid) (toSource attrs) 1
      pure a
    FailedThisSkillTest iid (isAbilitySource attrs 1 -> True) -> do
      pushAll
        [ AdditionalHealHorror (toTarget iid) (toSource attrs) 0
        , toMessage $ randomDiscard iid (toSource attrs)
        ]
      pure a
    _ -> LiquidCourage <$> runMessage msg attrs
