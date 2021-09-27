module Arkham.Types.Location.Cards.RehearsalRoom
  ( rehearsalRoom
  , RehearsalRoom(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Location.Helpers
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Timing qualified as Timing

newtype RehearsalRoom = RehearsalRoom LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

rehearsalRoom :: LocationCard RehearsalRoom
rehearsalRoom =
  location RehearsalRoom Cards.rehearsalRoom 1 (PerPlayer 1) Moon [Diamond]

instance HasAbilities RehearsalRoom where
  getAbilities (RehearsalRoom attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1 $ ForcedAbility $ SkillTestResult
        Timing.After
        You
        (WhileInvestigating $ LocationWithId $ toId attrs)
        (SuccessResult $ AtLeast $ Static 2)
    | locationRevealed attrs
    ]

instance LocationRunner env => RunMessage env RehearsalRoom where
  runMessage msg l@(RehearsalRoom attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      l <$ push (InvestigatorAssignDamage iid source DamageAny 0 1)
    _ -> RehearsalRoom <$> runMessage msg attrs
