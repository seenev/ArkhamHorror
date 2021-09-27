module Arkham.Types.Event.Cards.ImOuttaHere
  ( imOuttaHere
  , ImOuttaHere(..)
  ) where

import Arkham.Prelude

import Arkham.Event.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Event.Attrs
import Arkham.Types.Message

newtype ImOuttaHere = ImOuttaHere EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

imOuttaHere :: EventCard ImOuttaHere
imOuttaHere = event ImOuttaHere Cards.imOuttaHere

instance RunMessage env ImOuttaHere where
  runMessage msg e@(ImOuttaHere attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ | eid == toId attrs ->
      e <$ push (Resign iid)
    _ -> ImOuttaHere <$> runMessage msg attrs
