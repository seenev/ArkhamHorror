module Arkham.Asset.Assets.SurvivalKnife (survivalKnife, SurvivalKnife (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Fight
import Arkham.Id
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Window (WindowType)
import Arkham.Window qualified as Window

newtype SurvivalKnife = SurvivalKnife AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

survivalKnife :: AssetCard SurvivalKnife
survivalKnife = asset SurvivalKnife Cards.survivalKnife

instance HasAbilities SurvivalKnife where
  getAbilities (SurvivalKnife a) =
    [ fightAbility a 1 mempty ControlsThis
    , restrictedAbility a 2 (ControlsThis <> DuringPhase #enemy)
        $ ReactionAbility (DealtDamage #after (SourceIsEnemyAttack AnyEnemy) You) (exhaust a)
    ]

toEnemy :: [WindowType] -> EnemyId
toEnemy [] = error "called during incorrect window"
toEnemy (Window.DealtDamage (EnemyAttackSource eid) _ _ _ : _) = eid
toEnemy (_ : xs) = toEnemy xs

instance RunMessage SurvivalKnife where
  runMessage msg a@(SurvivalKnife attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      let source = toAbilitySource attrs 1
      sid <- getRandom
      chooseFight <- toMessage <$> mkChooseFight sid iid source
      enabled <- skillTestModifiers sid source iid [SkillModifier #combat 1]
      pushAll [enabled, chooseFight]
      pure a
    UseCardAbility iid (isSource attrs -> True) 2 windows' _ -> do
      let source = toAbilitySource attrs 2
      sid <- getRandom
      enabled <- skillTestModifiers sid source iid [SkillModifier #combat 2, DamageDealt 1]

      pushAll
        [ enabled
        , FightEnemy sid iid (toEnemy $ map Window.windowType windows') source Nothing #combat False
        ]
      pure a
    _ -> SurvivalKnife <$> runMessage msg attrs
