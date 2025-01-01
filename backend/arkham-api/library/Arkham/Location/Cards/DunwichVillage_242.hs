module Arkham.Location.Cards.DunwichVillage_242 (
  dunwichVillage_242,
  DunwichVillage_242 (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Classes
import Arkham.Exception
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (dunwichVillage_242)
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Trait

newtype DunwichVillage_242 = DunwichVillage_242 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

dunwichVillage_242 :: LocationCard DunwichVillage_242
dunwichVillage_242 =
  location DunwichVillage_242 Cards.dunwichVillage_242 3 (Static 1)

instance HasAbilities DunwichVillage_242 where
  getAbilities (DunwichVillage_242 attrs) =
    withResignAction
      attrs
      [ limitedAbility (GroupLimit PerGame 1)
        $ restrictedAbility
          attrs
          1
          ( Here
              <> InvestigatorExists (You <> InvestigatorWithAnyClues)
              <> EnemyCriteria (EnemyExists $ EnemyWithTrait Abomination)
          )
          (FastAbility Free)
      | locationRevealed attrs
      ]

instance RunMessage DunwichVillage_242 where
  runMessage msg l@(DunwichVillage_242 attrs) = case msg of
    UseCardAbility _ source 1 _ _ | isSource attrs source -> do
      investigatorsWithClues <- locationInvestigatorsWithClues attrs
      investigatorPlayersWithClues <- traverse (traverseToSnd getPlayer) investigatorsWithClues
      abominations <- select $ EnemyWithTrait Abomination
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
    _ -> DunwichVillage_242 <$> runMessage msg attrs
