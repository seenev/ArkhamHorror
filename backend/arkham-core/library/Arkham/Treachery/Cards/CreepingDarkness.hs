module Arkham.Treachery.Cards.CreepingDarkness (
  creepingDarkness,
  CreepingDarkness (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype CreepingDarkness = CreepingDarkness TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

creepingDarkness :: TreacheryCard CreepingDarkness
creepingDarkness = treachery CreepingDarkness Cards.creepingDarkness

instance HasModifiersFor CreepingDarkness where
  getModifiersFor (EnemyTarget eid) (CreepingDarkness a) = do
    isFormlessSpawn <- eid <=~> enemyIs Enemies.formlessSpawn
    n <- getPlayerCountValue (PerPlayer 1)
    pure $ toModifiers a [HealthModifier n | isFormlessSpawn]
  getModifiersFor _ _ = pure []

instance HasAbilities CreepingDarkness where
  getAbilities (CreepingDarkness a) =
    [restrictedAbility a 1 OnSameLocation $ ActionAbility [] $ ActionCost 2]

instance RunMessage CreepingDarkness where
  runMessage msg t@(CreepingDarkness attrs) = case msg of
    Revelation _ source | isSource attrs source -> do
      nexus <- selectJust $ locationIs Locations.nexusOfNKai
      pushAll
        [ attachTreachery attrs nexus
        , PlaceDoom (toSource attrs) (toTarget attrs) 1
        ]
      pure t
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      hasTorches <- getHasSupply iid Torches
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label
          "Test {willpower} (3)"
          [beginSkillTest iid (attrs.ability 1) attrs #willpower (Fixed 3)]
        : [Label "Check supplies" [toDiscardBy iid (toAbilitySource attrs 1) (toTarget attrs)] | hasTorches]
      pure t
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        push $ toDiscardBy iid (toAbilitySource attrs 1) attrs
        pure t
    _ -> CreepingDarkness <$> runMessage msg attrs
