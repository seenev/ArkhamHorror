module Arkham.Event.Events.Backstab where

import Arkham.Aspect hiding (aspect)
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Fight
import Arkham.Modifier

newtype Backstab = Backstab EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

backstab :: EventCard Backstab
backstab = event Backstab Cards.backstab

instance RunMessage Backstab where
  runMessage msg e@(Backstab attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | attrs `is` eid -> do
      sid <- getRandom
      skillTestModifier sid attrs iid (DamageDealt 2)
      aspect iid attrs (#agility `InsteadOf` #combat) (mkChooseFight sid iid attrs)
      pure e
    _ -> Backstab <$> liftRunMessage msg attrs
