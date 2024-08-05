module Arkham.Treachery.Cards.LostHumanity (lostHumanity, LostHumanity (..)) where

import Arkham.Classes
import Arkham.Investigator.Types (Field (..))
import Arkham.Prelude
import Arkham.Projection
import Arkham.Treachery.Cards qualified as Cards
import Arkham.Treachery.Runner

newtype LostHumanity = LostHumanity TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lostHumanity :: TreacheryCard LostHumanity
lostHumanity = treachery LostHumanity Cards.lostHumanity

instance RunMessage LostHumanity where
  runMessage msg t@(LostHumanity attrs) = case msg of
    Revelation iid (isSource attrs -> True) -> do
      sid <- getRandom
      pushAll
        [ revelationSkillTest sid iid attrs #willpower (Fixed 5)
        , RevelationChoice iid (toSource attrs) 1
        ]
      pure t
    RevelationChoice iid (isSource attrs -> True) 1 -> do
      handCount <- fieldMap InvestigatorHand length iid
      deckCount <- fieldMap InvestigatorDeck length iid
      discardCount <- fieldMap InvestigatorDiscard length iid
      when (handCount + deckCount + discardCount < 10) $ push $ DrivenInsane iid
      pure t
    FailedThisSkillTestBy iid (isSource attrs -> True) n -> do
      push $ DiscardTopOfDeck iid n (toSource attrs) Nothing
      pure t
    _ -> LostHumanity <$> runMessage msg attrs
