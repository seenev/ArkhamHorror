module Arkham.Location.Cards.DevilsHopYard_253 (
  devilsHopYard_253,
  DevilsHopYard_253 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Exception
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (devilsHopYard_253)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype DevilsHopYard_253 = DevilsHopYard_253 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

devilsHopYard_253 :: LocationCard DevilsHopYard_253
devilsHopYard_253 =
  location DevilsHopYard_253 Cards.devilsHopYard_253 2 (PerPlayer 1)

instance HasAbilities DevilsHopYard_253 where
  getAbilities (DevilsHopYard_253 attrs) =
    withRevealedAbilities
      attrs
      [ limitedAbility (GroupLimit PerGame 1)
          $ restrictedAbility
            attrs
            1
            ( Here
                <> InvestigatorExists
                  (InvestigatorAt YourLocation <> InvestigatorWithAnyClues)
                <> EnemyCriteria
                  ( EnemyExists $ EnemyAt YourLocation <> EnemyWithTrait Abomination
                  )
            )
            (FastAbility Free)
      ]

instance RunMessage DevilsHopYard_253 where
  runMessage msg l@(DevilsHopYard_253 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      investigatorsWithClues <- locationInvestigatorsWithClues attrs
      investigatorPlayersWithClues <- traverse (traverseToSnd getPlayer) investigatorsWithClues
      abominations <- locationEnemiesWithTrait attrs Abomination
      when
        (null investigatorsWithClues || null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      pushAll
        [ chooseOne
          player
          [ Label
              "Place clue on Abomination"
              [ chooseOne
                  player
                  [ targetLabel
                    eid
                    [ PlaceClues (toAbilitySource attrs 1) (toTarget eid) 1
                    , InvestigatorSpendClues iid 1
                    ]
                  | eid <- abominations
                  ]
              ]
          , Label "Do not place clue on Abomination" []
          ]
        | (iid, player) <- investigatorPlayersWithClues
        ]
      pure l
    _ -> DevilsHopYard_253 <$> runMessage msg attrs
