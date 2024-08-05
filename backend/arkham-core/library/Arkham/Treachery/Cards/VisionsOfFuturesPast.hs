module Arkham.Treachery.Cards.VisionsOfFuturesPast (VisionsOfFuturesPast (..), visionsOfFuturesPast) where

import Arkham.Classes
import Arkham.Prelude
import Arkham.Source
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype VisionsOfFuturesPast = VisionsOfFuturesPast TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

visionsOfFuturesPast :: TreacheryCard VisionsOfFuturesPast
visionsOfFuturesPast =
  treachery VisionsOfFuturesPast Cards.visionsOfFuturesPast

instance RunMessage VisionsOfFuturesPast where
  runMessage msg t@(VisionsOfFuturesPast attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed 5)
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ DiscardTopOfDeck iid n (toSource attrs) Nothing
      pure t
    _ -> VisionsOfFuturesPast <$> runMessage msg attrs
