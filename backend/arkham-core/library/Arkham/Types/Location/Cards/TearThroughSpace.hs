module Arkham.Types.Location.Cards.TearThroughSpace
  ( tearThroughSpace
  , TearThroughSpace(..)
  ) where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (tearThroughSpace)
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Id
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.Name
import Arkham.Types.Timing qualified as Timing
import Control.Monad.Extra (findM)

newtype TearThroughSpace = TearThroughSpace LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

tearThroughSpace :: LocationCard TearThroughSpace
tearThroughSpace = location
  TearThroughSpace
  Cards.tearThroughSpace
  1
  (Static 1)
  Square
  [Diamond, Triangle, Square]

instance HasAbilities TearThroughSpace where
  getAbilities (TearThroughSpace attrs) =
    withBaseAbilities attrs $
      [ mkAbility attrs 1 $ ForcedAbility $ RoundEnds Timing.When
      | locationRevealed attrs
      ]

instance LocationRunner env => RunMessage env TearThroughSpace where
  runMessage msg l@(TearThroughSpace attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source -> l <$ push
      (chooseOne
        iid
        [ Label
          "Place 1 doom on Tear through Space"
          [PlaceDoom (toTarget attrs) 1]
        , Label "Discard Tear through Space" [Discard (toTarget attrs)]
        ]
      )
    Revelation _ source | isSource attrs source -> do
      let
        labels = [ nameToLabel (toName attrs) <> tshow @Int n | n <- [1 .. 4] ]
      availableLabel <- findM
        (fmap isNothing . getId @(Maybe LocationId) . LocationWithLabel)
        labels
      case availableLabel of
        Just label -> pure . TearThroughSpace $ attrs & labelL .~ label
        Nothing -> error "could not find label"
    _ -> TearThroughSpace <$> runMessage msg attrs
