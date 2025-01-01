module Arkham.Location.Cards.TenAcreMeadow_247 (
  tenAcreMeadow_247,
  TenAcreMeadow_247 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Exception
import Arkham.Game.Helpers
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (tenAcreMeadow_247)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype TenAcreMeadow_247 = TenAcreMeadow_247 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tenAcreMeadow_247 :: LocationCard TenAcreMeadow_247
tenAcreMeadow_247 =
  location TenAcreMeadow_247 Cards.tenAcreMeadow_247 2 (Static 3)

instance HasAbilities TenAcreMeadow_247 where
  getAbilities (TenAcreMeadow_247 attrs) =
    withBaseAbilities
      attrs
      [ limitedAbility (GroupLimit PerGame 1)
        $ restrictedAbility
          attrs
          1
          ( Here
              <> exists (InvestigatorAt YourLocation <> InvestigatorWithAnyClues)
              <> exists (EnemyAt YourLocation <> EnemyWithTrait Abomination)
          )
          (FastAbility Free)
      | locationRevealed attrs
      ]

instance RunMessage TenAcreMeadow_247 where
  runMessage msg l@(TenAcreMeadow_247 attrs) = case msg of
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
                    [ PlaceClues (toAbilitySource attrs 1) (EnemyTarget eid) 1
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
    _ -> TenAcreMeadow_247 <$> runMessage msg attrs
