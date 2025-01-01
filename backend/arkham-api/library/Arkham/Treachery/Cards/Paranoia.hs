module Arkham.Treachery.Cards.Paranoia where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype Paranoia = Paranoia TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

paranoia :: TreacheryCard Paranoia
paranoia = treachery Paranoia Cards.paranoia

instance RunMessage Paranoia where
  runMessage msg t@(Paranoia attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      resourceCount' <- field InvestigatorResources iid
      push $ LoseResources iid (toSource attrs) resourceCount'
      pure t
    _ -> Paranoia <$> runMessage msg attrs
