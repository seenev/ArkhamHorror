module Arkham.Event.Events.DecipheredReality5 (decipheredReality5, DecipheredReality5 (..)) where

import Arkham.Action qualified as Action
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Investigate
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Modifier
import Arkham.Projection

newtype DecipheredReality5 = DecipheredReality5 EventAttrs
  deriving anyclass (IsEvent, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

decipheredReality5 :: EventCard DecipheredReality5
decipheredReality5 = event DecipheredReality5 Cards.decipheredReality5

instance RunMessage DecipheredReality5 where
  runMessage msg e@(DecipheredReality5 attrs) = runQueueT $ case msg of
    PlayThisEvent iid eid | eid == toId attrs -> do
      locationIds <- select RevealedLocation
      maxShroud <- maximum . ncons 0 <$> traverse (fieldMap LocationShroud (fromMaybe 0)) locationIds
      sid <- getRandom
      skillTestModifier sid attrs sid (SetDifficulty maxShroud)
      push . Msg.toMessage =<< (mkInvestigate sid iid attrs <&> setTarget attrs)
      pure e
    Successful (Action.Investigate, actionTarget) iid source target n | isTarget attrs target -> do
      -- Deciphered Reality is not a replacement effect; its effect doesn’t use
      -- any form of ‘instead’ or ‘but,’ so its effect is in addition to the
      -- standard rewards for successfully investigating.
      locationIds <- select RevealedLocation
      pushAll
        $ Successful (Action.Investigate, actionTarget) iid source actionTarget n
        : [ Msg.DiscoverClues iid $ discover lid' (toSource attrs) 1
          | lid' <- locationIds
          ]
      pure e
    _ -> DecipheredReality5 <$> liftRunMessage msg attrs
