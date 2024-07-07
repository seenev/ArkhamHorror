module Arkham.Event.Cards.Lucky3 where

import Arkham.Prelude

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Modifiers
import Arkham.SkillTest.Base

newtype Lucky3 = Lucky3 EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

lucky3 :: EventCard Lucky3
lucky3 = event Lucky3 Cards.lucky3

instance RunMessage Lucky3 where
  runMessage msg e@(Lucky3 attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == eventId attrs -> do
      mSkillTest <- getSkillTest
      case mSkillTest of
        Nothing -> error "invalid call"
        Just skillTest -> do
          let
            iid' = skillTestInvestigator skillTest
          let drawing = drawCards iid attrs 1
          pushAll
            [ drawing
            , skillTestModifier attrs iid' (AnySkillValue 3)
            , RerunSkillTest
            ]
      pure e
    _ -> Lucky3 <$> runMessage msg attrs
