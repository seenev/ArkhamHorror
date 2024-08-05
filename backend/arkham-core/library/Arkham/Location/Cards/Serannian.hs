module Arkham.Location.Cards.Serannian (serannian, Serannian (..)) where

import Arkham.GameValue
import Arkham.Helpers.Modifiers
import Arkham.Helpers.Story
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Prelude
import Arkham.Story.Cards qualified as Story

newtype Serannian = Serannian LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

serannian :: LocationCard Serannian
serannian = location Serannian Cards.serannian 3 (PerPlayer 1)

instance HasModifiersFor Serannian where
  getModifiersFor target (Serannian attrs) | attrs `is` target = do
    pure
      $ toModifiers
        attrs
        [ AdditionalCostToEnter $ HandDiscardCost 1 #any
        , AdditionalCostToLeave $ HandDiscardCost 1 #any
        ]
  getModifiersFor _ _ = pure []

instance HasAbilities Serannian where
  getAbilities (Serannian attrs) = veiled attrs []

instance RunMessage Serannian where
  runMessage msg (Serannian attrs) = case msg of
    Flip iid _ (isTarget attrs -> True) -> do
      readStory iid (toId attrs) Story.timelessBeauty
      pure . Serannian $ attrs & canBeFlippedL .~ False
    _ -> Serannian <$> runMessage msg attrs
