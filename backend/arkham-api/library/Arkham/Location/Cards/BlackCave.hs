module Arkham.Location.Cards.BlackCave (
  blackCave,
  BlackCave (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Investigator.Types
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Projection
import Arkham.Timing qualified as Timing

newtype BlackCave = BlackCave LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blackCave :: LocationCard BlackCave
blackCave = location BlackCave Cards.blackCave 3 (PerPlayer 2)

instance HasAbilities BlackCave where
  getAbilities (BlackCave attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here
          $ ForcedAbility
          $ TurnEnds
            Timing.When
            You
        | locationRevealed attrs
        ]

instance RunMessage BlackCave where
  runMessage msg l@(BlackCave attrs) = case msg of
    UseCardAbility iid source 1 _ _ | isSource attrs source -> do
      cardsInHand <- fieldMap InvestigatorHand length iid
      player <- getPlayer iid
      push
        $ chooseOrRunOne player
        $ Label
          "Take 1 horror"
          [InvestigatorAssignDamage iid source DamageAny 0 1]
        : [ Label
            "Choose and discard 2 cards from your hand"
            [ toMessage $ chooseAndDiscardCard iid (toAbilitySource attrs 1)
            , toMessage $ chooseAndDiscardCard iid (toAbilitySource attrs 1)
            ]
          | cardsInHand >= 2
          ]
      pure l
    _ -> BlackCave <$> runMessage msg attrs
