module Arkham.Treachery.Cards.FateOfAllFools (fateOfAllFools, FateOfAllFools (..)) where

import Arkham.Classes
import Arkham.Helpers.Message qualified as Msg
import Arkham.Matcher
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype FateOfAllFools = FateOfAllFools TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

fateOfAllFools :: TreacheryCard FateOfAllFools
fateOfAllFools = treachery FateOfAllFools Cards.fateOfAllFools

instance RunMessage FateOfAllFools where
  runMessage msg t@(FateOfAllFools attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      selectOne (treacheryIs Cards.fateOfAllFools <> TreacheryInThreatAreaOf Anyone) >>= \case
        Just tid -> do
          iid' <- selectJust $ HasMatchingTreachery $ TreacheryWithId tid
          chooseOne
            iid
            [ Label
                "An investigator with another copy of Fate of All Fools in his or her threat area takes 2 direct damage."
                [Msg.directDamage iid' attrs 2]
            , Label
                "Place 1 doom on another copy of Fate of All Fools."
                [PlaceDoom (toSource attrs) (toTarget tid) 1]
            ]
        Nothing -> placeInThreatArea attrs iid
      pure t
    _ -> FateOfAllFools <$> liftRunMessage msg attrs
