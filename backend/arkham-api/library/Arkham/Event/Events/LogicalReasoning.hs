module Arkham.Event.Events.LogicalReasoning (
  logicalReasoning,
  LogicalReasoning (..),
) where

import Arkham.Prelude hiding (terror)

import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Matcher
import Arkham.Trait

newtype LogicalReasoning = LogicalReasoning EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

logicalReasoning :: EventCard LogicalReasoning
logicalReasoning = event LogicalReasoning Cards.logicalReasoning

instance RunMessage LogicalReasoning where
  runMessage msg e@(LogicalReasoning attrs) = case msg of
    InvestigatorPlayEvent iid eid _ _ _ | eid == toId attrs -> do
      iids <- select =<< guardAffectsColocated iid
      options <- for iids \iid' -> do
        canHealHorror <- canHaveHorrorHealed attrs iid'
        terrors <- select $ TreacheryWithTrait Terror <> treacheryInThreatAreaOf iid'
        player <- getPlayer iid'
        pure
          ( iid'
          , player
          , [Label "Heal 2 Horror" [HealHorror (toTarget iid') (toSource attrs) 2] | canHealHorror]
              <> [ Label
                  "Discard a Terror"
                  [ chooseOne player
                      $ targetLabels terrors
                      $ only
                      . toDiscardBy iid attrs
                  ]
                 | notNull terrors
                 ]
          )
      let
        choices = flip mapMaybe options $ \(iid', player, choices') ->
          case choices' of
            [] -> Nothing
            _ -> Just $ targetLabel iid' [chooseOrRunOne player choices']
      player <- getPlayer iid
      push $ chooseOrRunOne player choices
      pure e
    _ -> LogicalReasoning <$> runMessage msg attrs
