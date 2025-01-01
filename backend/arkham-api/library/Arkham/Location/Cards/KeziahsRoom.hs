module Arkham.Location.Cards.KeziahsRoom (keziahsRoom, KeziahsRoom (..)) where

import Arkham.Ability
import Arkham.Action qualified as Action
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Helpers.Ability
import Arkham.Location.Cards qualified as Cards
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Movement
import Arkham.Prelude
import Arkham.Scenario.Deck

newtype Metadata = Metadata {revealTopCard :: Bool}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype KeziahsRoom = KeziahsRoom (LocationAttrs `With` Metadata)
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

keziahsRoom :: LocationCard KeziahsRoom
keziahsRoom =
  location (KeziahsRoom . (`with` Metadata False)) Cards.keziahsRoom 3 (Static 0)

instance HasAbilities KeziahsRoom where
  getAbilities (KeziahsRoom (a `With` _)) =
    withBaseAbilities
      a
      [ mkAbility a 1
          $ freeReaction
          $ SkillTestResult #when You (WhileInvestigating $ be a) #success
      ]

instance RunMessage KeziahsRoom where
  runMessage msg l@(KeziahsRoom (attrs `With` meta)) = case msg of
    UseCardAbility _iid (isSource attrs -> True) 1 _ _ -> do
      pure $ KeziahsRoom $ attrs `with` Metadata True
    Successful (Action.Investigate, _) iid _ (isTarget attrs -> True) _ | revealTopCard meta -> do
      push $ DrawCards iid $ targetCardDraw attrs UnknownPlacesDeck 1
      pure $ KeziahsRoom $ attrs `with` Metadata False
    DrewCards iid drewCards | maybe False (isTarget attrs) drewCards.target -> do
      labels <-
        selectFields LocationLabel
          $ LocationWithUnrevealedTitle "Unknown Places"
          <> NotLocation RevealedLocation
      let
        nextLabel =
          fromJustNote "too many locations"
            $ find (`notElem` labels)
            $ map
              (\n -> "unknownPlaces" <> tshow n)
              ([1 .. 7] :: [Int])
      for_ drewCards.cards $ \card -> do
        (lid, placement) <- placeLocation card
        player <- getPlayer iid
        pushAll
          [ placement
          , SetLocationLabel lid nextLabel
          , chooseOne
              player
              [ Label "Do not move" []
              , Label "Move to location" [Move $ move attrs iid lid]
              ]
          ]
      pure l
    _ -> KeziahsRoom . (`with` meta) <$> runMessage msg attrs
