module Arkham.Location.Cards.EerieGlade (
  eerieGlade,
  EerieGlade (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.GameValue
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Cards qualified as Cards (eerieGlade)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype EerieGlade = EerieGlade LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

eerieGlade :: LocationCard EerieGlade
eerieGlade = location EerieGlade Cards.eerieGlade 4 (PerPlayer 1)

instance HasAbilities EerieGlade where
  getAbilities (EerieGlade attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility
          attrs
          1
          (InvestigatorExists $ You <> InvestigatorWithAnyActionsRemaining)
          $ ForcedAbility
          $ RevealLocation Timing.After You
          $ LocationWithId
          $ toId attrs
        | locationRevealed attrs
        ]

instance RunMessage EerieGlade where
  runMessage msg l@(EerieGlade attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      actionRemainingCount <- field InvestigatorRemainingActions iid
      push
        $ DiscardTopOfDeck
          iid
          (actionRemainingCount * 2)
          (toAbilitySource attrs 1)
          Nothing
      pure l
    _ -> EerieGlade <$> runMessage msg attrs
