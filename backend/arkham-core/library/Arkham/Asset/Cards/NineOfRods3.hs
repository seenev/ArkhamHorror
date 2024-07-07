module Arkham.Asset.Cards.NineOfRods3 (nineOfRods3, NineOfRods3 (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Deck qualified as Deck
import Arkham.Helpers.Card (getCardEntityTarget)
import Arkham.Matcher
import Arkham.Prelude

newtype NineOfRods3 = NineOfRods3 AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

nineOfRods3 :: AssetCard NineOfRods3
nineOfRods3 = asset NineOfRods3 Cards.nineOfRods3

instance HasAbilities NineOfRods3 where
  getAbilities (NineOfRods3 a) =
    [ reactionAbility
        a
        1
        (exhaust a)
        (DrawCard #when You (CanCancelRevelationEffect $ basic NonWeaknessTreachery) EncounterDeck)
        ControlsThis
    , restrictedAbility a 2 InYourHand $ freeReaction (GameBegins #when)
    ]

instance RunMessage NineOfRods3 where
  runMessage msg a@(NineOfRods3 attrs) = case msg of
    UseCardAbility iid (isSource attrs -> True) 1 (cardDrawn -> card) _ -> do
      target <- getCardEntityTarget card
      pushAll
        [ CancelNext (toSource attrs) RevelationMessage
        , CancelNext (toSource attrs) DrawEnemyMessage
        , CancelSurge (toSource attrs)
        , ShuffleIntoDeck Deck.EncounterDeck target
        , drawEncounterCard iid attrs
        ]
      pure a
    InHand _ (UseThisAbility iid (isSource attrs -> True) 2) -> do
      push $ putCardIntoPlay iid attrs
      pure a
    _ -> NineOfRods3 <$> runMessage msg attrs
