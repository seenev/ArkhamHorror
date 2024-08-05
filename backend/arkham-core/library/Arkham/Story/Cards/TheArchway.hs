module Arkham.Story.Cards.TheArchway (TheArchway (..), theArchway) where

import Arkham.Helpers.Query
import Arkham.Helpers.SkillTest
import Arkham.Location.Cards qualified as Locations
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Story.Cards qualified as Cards
import Arkham.Story.Runner

newtype TheArchway = TheArchway StoryAttrs
  deriving anyclass (IsStory, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

theArchway :: StoryCard TheArchway
theArchway = story TheArchway Cards.theArchway

instance RunMessage TheArchway where
  runMessage msg s@(TheArchway attrs) = case msg of
    ResolveStory iid _ story' | story' == toId attrs -> do
      setAsideDimStreets <- getSetAsideCardsMatching $ CardWithTitle "Dim Streets"
      otherDimStreets <- case nonEmpty setAsideDimStreets of
        Nothing -> error "missing"
        Just xs -> sample xs
      dimStreets <- selectJust $ locationIs Locations.dimStreetsTheArchway
      sid <- getRandom
      pushAll
        [ beginSkillTest sid iid attrs iid #intellect (Fixed 3)
        , ReplaceLocation dimStreets otherDimStreets DefaultReplace
        ]
      pure s
    _ -> TheArchway <$> runMessage msg attrs
