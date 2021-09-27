module Arkham.Types.Location.Cards.ExhibitHallMedusaExhibit
  ( exhibitHallMedusaExhibit
  , ExhibitHallMedusaExhibit(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (exhibitHallMedusaExhibit)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype ExhibitHallMedusaExhibit = ExhibitHallMedusaExhibit LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

exhibitHallMedusaExhibit :: LocationCard ExhibitHallMedusaExhibit
exhibitHallMedusaExhibit = locationWithRevealedSideConnections
  ExhibitHallMedusaExhibit
  Cards.exhibitHallMedusaExhibit
  2
  (PerPlayer 1)
  NoSymbol
  [Square]
  T
  [Square, Moon]

instance HasAbilities ExhibitHallMedusaExhibit where
  getAbilities (ExhibitHallMedusaExhibit x) = withBaseAbilities
    x
    [ mkAbility x 1
      $ ForcedAbility
      $ SkillTestResult
          Timing.After
          You
          (WhileInvestigating $ LocationWithId $ toId x)
      $ FailureResult AnyValue
    | locationRevealed x
    ]

instance LocationRunner env => RunMessage env ExhibitHallMedusaExhibit where
  runMessage msg l@(ExhibitHallMedusaExhibit attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (ChooseAndDiscardAsset iid AnyAsset)
    _ -> ExhibitHallMedusaExhibit <$> runMessage msg attrs
