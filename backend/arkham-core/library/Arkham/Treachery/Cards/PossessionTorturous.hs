module Arkham.Treachery.Cards.PossessionTorturous (possessionTorturous, PossessionTorturous (..)) where

import Arkham.Ability
import Arkham.Investigator.Types (Field (..))
import Arkham.Placement
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Import.Lifted

newtype PossessionTorturous = PossessionTorturous TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

possessionTorturous :: TreacheryCard PossessionTorturous
possessionTorturous = treachery PossessionTorturous Cards.possessionTorturous

instance HasAbilities PossessionTorturous where
  getAbilities (PossessionTorturous a) =
    [restrictedAbility a 1 InYourHand $ actionAbilityWithCost $ ResourceCost 5]

instance RunMessage PossessionTorturous where
  runMessage msg t@(PossessionTorturous attrs) = runQueueT $ case msg of
    Revelation iid (isSource attrs -> True) -> do
      horror <- field InvestigatorHorror iid
      sanity <- field InvestigatorSanity iid
      when (horror > sanity * 2) $ kill attrs iid
      placeTreachery attrs (HiddenInHand iid)
      pure t
    EndCheckWindow {} -> case treacheryInHandOf attrs of
      Just iid -> do
        horror <- field InvestigatorHorror iid
        sanity <- field InvestigatorSanity iid
        when (horror > sanity * 2) $ kill attrs iid
        pure t
      Nothing -> pure t
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      toDiscardBy iid (attrs.ability 1) attrs
      pure t
    _ -> PossessionTorturous <$> liftRunMessage msg attrs
