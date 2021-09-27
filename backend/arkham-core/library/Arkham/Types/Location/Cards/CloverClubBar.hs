module Arkham.Types.Location.Cards.CloverClubBar
  ( cloverClubBar
  , CloverClubBar(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (cloverClubBar)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Message
import Arkham.Types.ScenarioLogKey

newtype CloverClubBar = CloverClubBar LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

cloverClubBar :: LocationCard CloverClubBar
cloverClubBar = location
  CloverClubBar
  Cards.cloverClubBar
  3
  (Static 0)
  Square
  [Triangle, Circle]

instance HasAbilities CloverClubBar where
  getAbilities (CloverClubBar attrs) = withBaseAbilities
    attrs
    [ restrictedAbility
          attrs
          1
          (OnAct 1)
          (ActionAbility Nothing $ Costs [ActionCost 1, ResourceCost 2])
        & (abilityLimitL .~ PlayerLimit PerGame 1)
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env CloverClubBar where
  runMessage msg l@(CloverClubBar attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ pushAll
      [GainClues iid 2, DrawCards iid 2 False, Remember $ HadADrink iid]
    _ -> CloverClubBar <$> runMessage msg attrs
