module Arkham.Event.Cards.YouOweMeOne (
  youOweMeOne,
  YouOweMeOne (..),
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.Classes
import Arkham.Cost
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Investigator.Types (Field (InvestigatorHand))
import Arkham.Matcher
import Arkham.Projection
import Arkham.Window (defaultWindows)

newtype YouOweMeOne = YouOweMeOne EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

youOweMeOne :: EventCard YouOweMeOne
youOweMeOne = event YouOweMeOne Cards.youOweMeOne

instance RunMessage YouOweMeOne where
  runMessage msg e@(YouOweMeOne attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      others <- select $ NotInvestigator (InvestigatorWithId iid)
      player <- getPlayer iid
      push
        $ chooseOrRunOne
          player
          [ targetLabel
            other
            [HandleTargetChoice iid (toSource attrs) (InvestigatorTarget other)]
          | other <- others
          ]
      pure e
    HandleTargetChoice iid (isSource attrs -> True) (InvestigatorTarget iid') ->
      do
        cards <- field InvestigatorHand iid'
        let relevantCards = filter (`cardMatch` (NonWeakness <> NonSignature)) cards
        let drawing1 = drawCards iid attrs 1
        let drawing2 = drawCards iid' attrs 1
        player <- getPlayer iid
        pushAll
          [ FocusCards cards
          , chooseOne player
              $ Label "Do not play a card" []
              : [ targetLabel
                  (toCardId card)
                  [ RemoveCardFromHand iid' (toCardId card)
                  , AddToHand iid [card]
                  , InitiatePlayCard iid card Nothing NoPayment (defaultWindows iid) False
                  , drawing1
                  , drawing2
                  ]
                | card <- relevantCards
                ]
          , UnfocusCards
          ]
        pure e
    _ -> YouOweMeOne <$> runMessage msg attrs
