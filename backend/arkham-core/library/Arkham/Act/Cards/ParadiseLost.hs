module Arkham.Act.Cards.ParadiseLost (
  ParadiseLost (..),
  paradiseLost,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Classes
import Arkham.Helpers.Investigator
import Arkham.Location.Cards qualified as Locations
import Arkham.Location.Types (Field (..))
import Arkham.Matcher hiding (LocationCard, PlaceUnderneath)
import Arkham.Movement
import Arkham.Projection
import Arkham.Resolution
import Arkham.SkillType
import Arkham.Trait (Trait (Shattered))

newtype ParadiseLost = ParadiseLost ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paradiseLost :: ActCard ParadiseLost
paradiseLost = act (4, A) ParadiseLost Cards.paradiseLost Nothing

instance HasAbilities ParadiseLost where
  getAbilities (ParadiseLost a)
    | onSide A a =
        [ skillTestAbility
            $ restrictedAbility
              a
              1
              ( InvestigatorExists
                  $ You
                  <> InvestigatorAt
                    (LocationWithoutClues <> LocationWithTrait Shattered)
              )
            $ ActionAbility []
            $ ActionCost 1
        , restrictedAbility
            a
            2
            ( AssetExists
                $ AssetWithTitle "Relic of Ages"
                <> AssetWithCardsUnderneath
                  (HasCard $ cardIs Locations.valusia)
            )
            $ Objective
            $ ForcedAbility AnyWindow
        ]
  getAbilities _ = []

instance RunMessage ParadiseLost where
  runMessage msg a@(ParadiseLost attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 _ _ -> do
      player <- getPlayer iid
      sid <- getRandom
      push
        $ chooseOne
          player
          [ SkillLabel skillType [beginSkillTest sid iid (attrs.ability 1) attrs skillType (Fixed 3)]
          | skillType <- [SkillWillpower, SkillIntellect]
          ]
      pure a
    PassedSkillTest iid _ (isAbilitySource attrs 1 -> True) SkillTestInitiatorTarget {} _ _ ->
      do
        lid <- getJustLocation iid
        card <- field LocationCard lid
        iids <- select $ colocatedWith iid
        enemyIds <- select $ UnengagedEnemy <> enemyAt lid
        aPocketInTime <- selectJust $ locationIs Locations.aPocketInTime
        relic <- selectJust $ AssetWithTitle "Relic of Ages"
        pushAll
          $ [MoveTo $ move (toSource attrs) iid' aPocketInTime | iid' <- iids]
          <> [EnemyMove eid lid | eid <- enemyIds]
          <> [RemoveLocation lid, PlaceUnderneath (AssetTarget relic) [card]]
        pure a
    UseCardAbility _ (isSource attrs -> True) 2 _ _ -> do
      push $ AdvanceAct (toId attrs) (toSource attrs) AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      push $ ScenarioResolution $ Resolution 2
      pure a
    _ -> ParadiseLost <$> runMessage msg attrs
