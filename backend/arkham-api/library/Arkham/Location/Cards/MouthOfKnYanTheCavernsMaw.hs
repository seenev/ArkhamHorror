module Arkham.Location.Cards.MouthOfKnYanTheCavernsMaw (
  mouthOfKnYanTheCavernsMaw,
  MouthOfKnYanTheCavernsMaw (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Campaigns.TheForgottenAge.Helpers
import Arkham.Campaigns.TheForgottenAge.Supply
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Scenario.Deck

newtype MouthOfKnYanTheCavernsMaw = MouthOfKnYanTheCavernsMaw LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

mouthOfKnYanTheCavernsMaw :: LocationCard MouthOfKnYanTheCavernsMaw
mouthOfKnYanTheCavernsMaw = location MouthOfKnYanTheCavernsMaw Cards.mouthOfKnYanTheCavernsMaw 2 (Static 0)

instance HasAbilities MouthOfKnYanTheCavernsMaw where
  getAbilities (MouthOfKnYanTheCavernsMaw attrs) =
    withBaseAbilities attrs
      $ if locationRevealed attrs
        then
          [ withTooltip
              "Let's make camp and solve this puzzle tomorrow"
              (locationResignAction attrs)
          , restrictedAbility
              attrs
              2
              (Here <> HasSupply Compass)
              (ActionAbility [] $ ActionCost 1)
          ]
        else []

instance RunMessage MouthOfKnYanTheCavernsMaw where
  runMessage msg l@(MouthOfKnYanTheCavernsMaw attrs) = case msg of
    UseCardAbility iid source 2 _ _ | isSource attrs source -> do
      explorationDeck <- getExplorationDeck
      let
        (viewing, rest) = splitAt 3 explorationDeck
        cardPairs = map (toSnd (`deleteFirst` viewing)) viewing
      player <- getPlayer iid
      pushAll
        [ FocusCards viewing
        , SetScenarioDeck ExplorationDeck rest
        , questionLabel "Place one card on bottom of exploration deck" player
            $ ChooseOne
              [ targetLabel
                (toCardId c)
                [ PutCardOnBottomOfDeck
                    iid
                    (Deck.ScenarioDeckByKey ExplorationDeck)
                    c
                , FocusCards remaining
                , questionLabel "Place card on top of exploration deck" player
                    $ ChooseOneAtATime
                      [ targetLabel
                        (toCardId r)
                        [ PutCardOnTopOfDeck
                            iid
                            (Deck.ScenarioDeckByKey ExplorationDeck)
                            r
                        ]
                      | r <- remaining
                      ]
                ]
              | (c, remaining) <- cardPairs
              ]
        , UnfocusCards
        ]
      pure l
    _ -> MouthOfKnYanTheCavernsMaw <$> runMessage msg attrs
