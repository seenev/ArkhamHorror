module Arkham.Treachery.Cards.ChaosManifest (chaosManifest, ChaosManifest (..)) where

import Arkham.Classes
import Arkham.Helpers.Query
import Arkham.Prelude
import Arkham.Scenarios.InTheClutchesOfChaos.Helpers
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype ChaosManifest = ChaosManifest TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

chaosManifest :: TreacheryCard ChaosManifest
chaosManifest = treachery ChaosManifest Cards.chaosManifest

instance RunMessage ChaosManifest where
  runMessage msg t@(ChaosManifest attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      push $ revelationSkillTest sid iid attrs #willpower (Fixed 3)
      pure t
    FailedThisSkillTestBy _ (isSource attrs -> True) n -> do
      lead <- getLeadPlayer
      locations <- sampleLocations n
      push
        $ chooseOrRunOneAtATime lead
        $ [targetLabel location [PlaceBreaches (toTarget location) 1] | location <- locations]
      pure t
    _ -> ChaosManifest <$> runMessage msg attrs
