module Arkham.Types.Location.Cards.BlastedHeath_248
  ( blastedHeath_248
  , BlastedHeath_248(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (blastedHeath_248)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Cost
import Arkham.Types.Criteria
import Arkham.Types.Exception
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Query
import Arkham.Types.Target
import Arkham.Types.Trait

newtype BlastedHeath_248 = BlastedHeath_248 LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

blastedHeath_248 :: LocationCard BlastedHeath_248
blastedHeath_248 = location
  BlastedHeath_248
  Cards.blastedHeath_248
  4
  (Static 3)
  Square
  [Circle, Hourglass]

instance HasAbilities BlastedHeath_248 where
  getAbilities (BlastedHeath_248 attrs) =
    withBaseAbilities attrs $
      [ restrictedAbility
            attrs
            1
            (Here
            <> InvestigatorExists
                 (InvestigatorAt YourLocation <> InvestigatorWithAnyClues)
            <> EnemyCriteria
                 (EnemyExists
                 $ EnemyAt YourLocation
                 <> EnemyWithTrait Abomination
                 )
            )
            (FastAbility Free)
          & (abilityLimitL .~ GroupLimit PerGame 1)
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env BlastedHeath_248 where
  runMessage msg l@(BlastedHeath_248 attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> do
      investigatorWithCluePairs <- filter ((> 0) . snd) <$> traverse
        (traverseToSnd (fmap unClueCount . getCount))
        (setToList $ locationInvestigators attrs)
      abominations <-
        map EnemyTarget <$> locationEnemiesWithTrait attrs Abomination
      when
        (null investigatorWithCluePairs || null abominations)
        (throwIO $ InvalidState "should not have been able to use this ability")
      let
        totalClues = sum $ map snd investigatorWithCluePairs
        investigators = map fst investigatorWithCluePairs
        placeClueOnAbomination = chooseOne
          iid
          [ TargetLabel target [SpendClues 1 investigators, PlaceClues target 1]
          | target <- abominations
          ]

      l <$ pushAll
        ([placeClueOnAbomination]
        <> [ chooseOne
               iid
               [ Label "Spend a second clue" [placeClueOnAbomination]
               , Label "Do not spend a second clue" []
               ]
           | totalClues > 1
           ]
        )
    _ -> BlastedHeath_248 <$> runMessage msg attrs
