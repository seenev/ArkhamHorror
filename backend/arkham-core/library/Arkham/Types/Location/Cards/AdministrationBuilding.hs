module Arkham.Types.Location.Cards.AdministrationBuilding where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (administrationBuilding)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message hiding (RevealLocation)
import Arkham.Types.Timing qualified as Timing

newtype AdministrationBuilding = AdministrationBuilding LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

administrationBuilding :: LocationCard AdministrationBuilding
administrationBuilding = location
  AdministrationBuilding
  Cards.administrationBuilding
  4
  (PerPlayer 1)
  Circle
  [Plus, T]

instance HasAbilities AdministrationBuilding where
  getAbilities (AdministrationBuilding x) =
    withBaseAbilities x $ if locationRevealed x
      then
        [ restrictedAbility x 1 Here
        $ ForcedAbility
        $ RevealLocation Timing.After You
        $ LocationWithId
        $ toId x
        , restrictedAbility x 2 Here $ ForcedAbility $ TurnEnds Timing.When You
        ]
      else []

instance (LocationRunner env) => RunMessage env AdministrationBuilding where
  runMessage msg l@(AdministrationBuilding attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      l <$ push (PlaceLocationMatching $ CardWithTitle "Faculty Offices")
    UseCardAbility iid source _ 2 _ | isSource attrs source ->
      l <$ push (DiscardTopOfDeck iid 1 Nothing)
    _ -> AdministrationBuilding <$> runMessage msg attrs
