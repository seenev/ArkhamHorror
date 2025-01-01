module Arkham.Treachery.Cards.AncientEvils where

import Arkham.Classes
import Arkham.Prelude
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype AncientEvils = AncientEvils TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

ancientEvils :: TreacheryCard AncientEvils
ancientEvils = treachery AncientEvils Cards.ancientEvils

instance RunMessage AncientEvils where
  runMessage msg t@(AncientEvils attrs) = case msg of
    Revelation _ (isSource attrs -> True) -> do
      push placeDoomOnAgendaAndCheckAdvance
      pure t
    _ -> AncientEvils <$> runMessage msg attrs
