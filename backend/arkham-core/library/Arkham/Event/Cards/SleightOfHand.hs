module Arkham.Event.Cards.SleightOfHand (sleightOfHand, sleightOfHandEffect, SleightOfHand (..)) where

import Arkham.Cost
import Arkham.Cost.Status qualified as Cost
import Arkham.Effect.Import
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Import.Lifted
import Arkham.Helpers.Query (selectAssetController)
import Arkham.Matcher
import Arkham.Message qualified as Msg

newtype SleightOfHand = SleightOfHand EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleightOfHand :: EventCard SleightOfHand
sleightOfHand = event SleightOfHand Cards.sleightOfHand

instance RunMessage SleightOfHand where
  runMessage msg e@(SleightOfHand attrs) = runQueueT $ case msg of
    InvestigatorPlayEvent iid eid _ windows' _ | eid == toId attrs -> do
      cards <- select $ PlayableCard Cost.PaidCost $ inHandOf iid <> basic #item
      chooseOne
        iid
        [ targetLabel
          card
          [ PutCardIntoPlay iid card (Just $ toTarget attrs) NoPayment windows'
          , Msg.createCardEffect Cards.sleightOfHand Nothing attrs card
          ]
        | card <- cards
        ]
      pure e
    _ -> SleightOfHand <$> liftRunMessage msg attrs

newtype SleightOfHandEffect = SleightOfHandEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

sleightOfHandEffect :: EffectArgs -> SleightOfHandEffect
sleightOfHandEffect = cardEffect SleightOfHandEffect Cards.sleightOfHand

instance RunMessage SleightOfHandEffect where
  runMessage msg e@(SleightOfHandEffect attrs) = runQueueT $ case msg of
    EndTurn _ -> do
      case attrs.target of
        CardIdTarget cid -> do
          selectOne (AssetWithCardId cid) >>= traverse_ \aid -> selectAssetController aid >>= traverse_ \controller -> returnToHand controller aid
        _ -> pure ()
      disableReturn e
    _ -> SleightOfHandEffect <$> liftRunMessage msg attrs
