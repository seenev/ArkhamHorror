module Arkham.Types.Act.Cards.TheStrangerACityAflame
  ( TheStrangerACityAflame(..)
  , theStrangerACityAflame
  ) where

import Arkham.Prelude

import Arkham.Act.Cards qualified as Cards
import Arkham.Enemy.Cards qualified as Enemies
import Arkham.Scenarios.CurtainCall.Helpers
import Arkham.Types.Ability
import Arkham.Types.Act.Attrs
import Arkham.Types.Act.Runner
import Arkham.Types.Card
import Arkham.Types.Classes
import Arkham.Types.Matcher hiding (Discarded)
import Arkham.Types.Message
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing
import Arkham.Types.Token

newtype TheStrangerACityAflame = TheStrangerACityAflame ActAttrs
  deriving anyclass (IsAct, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theStrangerACityAflame :: ActCard TheStrangerACityAflame
theStrangerACityAflame =
  act (2, A) TheStrangerACityAflame Cards.theStrangerACityAflame Nothing

instance HasAbilities TheStrangerACityAflame where
  getAbilities (TheStrangerACityAflame a) =
    [ mkAbility a 1
        $ Objective
        $ ForcedAbility
        $ EnemyWouldBeDiscarded Timing.When
        $ enemyIs Enemies.theManInThePallidMask
    ]

instance ActRunner env => RunMessage env TheStrangerACityAflame where
  runMessage msg a@(TheStrangerACityAflame attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      a <$ push (AdvanceAct (toId attrs) source)
    AdvanceAct aid _ | aid == toId attrs && onSide B attrs -> do
      moveTheManInThePalidMaskToLobbyInsteadOfDiscarding
      theatre <- fromJustNote "theatre must be in play"
        <$> selectOne (LocationWithTitle "Theatre")
      card <- flipCard <$> genCard (toCardDef attrs)
      a <$ pushAll
        [ AddToken Cultist
        , AddToken Cultist
        , PlaceHorror (LocationTarget theatre) 1
        , PlaceNextTo ActDeckTarget [card]
        , CreateEffect "03047a" Nothing (toSource attrs) (toTarget attrs)
        , NextAct aid "03048"
        ]
    _ -> TheStrangerACityAflame <$> runMessage msg attrs
