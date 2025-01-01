module Arkham.Location.Cards.VaultsOfZin (vaultsOfZin) where

import Arkham.GameValue
import Arkham.Helpers.Story (readStory)
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Import.Lifted
import Arkham.Story.Cards qualified as Story

newtype VaultsOfZin = VaultsOfZin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

vaultsOfZin :: LocationCard VaultsOfZin
vaultsOfZin = location VaultsOfZin Cards.vaultsOfZin 3 (PerPlayer 1)

instance HasAbilities VaultsOfZin where
  getAbilities (VaultsOfZin attrs) = veiled attrs []

instance RunMessage VaultsOfZin where
  runMessage msg (VaultsOfZin attrs) = runQueueT $ case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.ghastlyTunnels
      pure . VaultsOfZin $ attrs & canBeFlippedL .~ False
    _ -> VaultsOfZin <$> liftRunMessage msg attrs
