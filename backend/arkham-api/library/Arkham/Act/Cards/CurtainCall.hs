module Arkham.Act.Cards.CurtainCall (
  CurtainCall (..),
  curtainCall,
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Act.Cards qualified as Cards
import Arkham.Act.Runner
import Arkham.Action qualified as Action
import Arkham.Classes
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Location.Cards qualified as Cards
import Arkham.Matcher
import Arkham.Resolution
import Arkham.Timing qualified as Timing

newtype CurtainCall = CurtainCall ActAttrs
  deriving anyclass (IsAct, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

curtainCall :: ActCard CurtainCall
curtainCall = act (3, A) CurtainCall Cards.curtainCall Nothing

instance HasAbilities CurtainCall where
  getAbilities (CurtainCall attrs) =
    [ restrictedAbility
        (proxied (LocationMatcherSource $ locationIs Cards.lobby) attrs)
        1
        ( Here
            <> Negate
              (EnemyCriteria $ EnemyExists $ enemyIs Enemies.theManInThePallidMask)
        )
        $ ActionAbility [Action.Resign]
        $ ActionCost 1
    , restrictedAbility
        attrs
        2
        (exists $ LocationWithoutHorror <> ConnectedTo LocationWithAnyHorror)
        $ ForcedAbility
        $ RoundEnds Timing.When
    ]
      <> [ restrictedAbility attrs 3 AllUndefeatedInvestigatorsResigned
          $ Objective
          $ ForcedAbility AnyWindow
         | onSide A attrs
         ]

instance RunMessage CurtainCall where
  runMessage msg a@(CurtainCall attrs) = case msg of
    UseCardAbility iid (ProxySource _ (isSource attrs -> True)) 1 _ _ -> do
      push $ Resign iid
      pure a
    UseCardAbility _ source 2 _ _ | isSource attrs source -> do
      targets <- selectTargets $ LocationWithoutHorror <> ConnectedTo LocationWithAnyHorror
      pushAll $ map (\t -> PlaceHorror (toAbilitySource attrs 3) t 1) targets
      pure a
    UseCardAbility _ source 3 _ _ | isSource attrs source -> do
      push $ AdvanceAct (toId attrs) source AdvancedWithOther
      pure a
    AdvanceAct aid _ _ | aid == toId attrs && onSide B attrs -> do
      lead <- getLeadPlayer
      push
        $ chooseOne
          lead
          [ Label
              "We have to warn the police about what's going on! (-> R1)"
              [ScenarioResolution $ Resolution 1]
          , Label
              "The police won't believe us. We have to solve this mystery on our own. (-> R2)"
              [ScenarioResolution $ Resolution 2]
          ]
      pure a
    _ -> CurtainCall <$> runMessage msg attrs
