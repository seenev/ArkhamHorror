module Arkham.Asset.Cards.ToothOfEztli (
  toothOfEztli,
  ToothOfEztli (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Matcher
import Arkham.Timing qualified as Timing

newtype ToothOfEztli = ToothOfEztli AssetAttrs
  deriving anyclass (IsAsset)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

toothOfEztli :: AssetCard ToothOfEztli
toothOfEztli = asset ToothOfEztli Cards.toothOfEztli

instance HasModifiersFor ToothOfEztli where
  getModifiersFor (InvestigatorTarget iid) (ToothOfEztli a) | controlledBy a iid = do
    mSource <- getSkillTestSource
    pure $ toModifiers a $ case mSource of
      Just (TreacherySource _) -> [SkillModifier #willpower 1, SkillModifier #agility 1]
      _ -> []
  getModifiersFor _ _ = pure []

instance HasAbilities ToothOfEztli where
  getAbilities (ToothOfEztli x) =
    [ restrictedAbility x 1 ControlsThis
        $ ReactionAbility
          (SkillTestResult Timing.After You (SkillTestOnTreachery AnyTreachery) (SuccessResult AnyValue))
          (exhaust x)
    ]

instance RunMessage ToothOfEztli where
  runMessage msg a@(ToothOfEztli attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      push $ drawCards iid (toAbilitySource attrs 1) 1
      pure a
    _ -> ToothOfEztli <$> runMessage msg attrs
