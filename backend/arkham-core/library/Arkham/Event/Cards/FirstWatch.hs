module Arkham.Event.Cards.FirstWatch (
  firstWatch,
  FirstWatch (..),
) where

import Arkham.Prelude

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Id

newtype FirstWatchMetadata = FirstWatchMetadata {firstWatchPairings :: [(InvestigatorId, EncounterCard)]}
  deriving newtype (Show, Eq, ToJSON, FromJSON)

newtype FirstWatch = FirstWatch (EventAttrs `With` FirstWatchMetadata)
  deriving anyclass (IsEvent, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

firstWatch :: EventCard FirstWatch
firstWatch =
  event (FirstWatch . (`with` FirstWatchMetadata [])) Cards.firstWatch

instance RunMessage FirstWatch where
  runMessage msg e@(FirstWatch (attrs@EventAttrs {..} `With` metadata@FirstWatchMetadata {..})) =
    case msg of
      InvestigatorPlayEvent _ eid _ _ _ | eid == eventId -> do
        withQueue_ $ \case
          (AllDrawEncounterCard : rest) -> rest
          _ -> error "AllDrawEncounterCard expected"
        playerCount <- getPlayerCount
        push $ DrawEncounterCards (EventTarget eventId) playerCount
        pure e
      UseCardAbilityChoice iid (EventSource eid) 1 (EncounterCardMetadata card) [] _ | eid == eventId -> do
        investigatorIds <-
          setFromList @(Set InvestigatorId) <$> getInvestigatorIds
        let
          assignedInvestigatorIds = setFromList $ map fst firstWatchPairings
          remainingInvestigatorIds =
            setToList
              . insertSet iid
              $ investigatorIds
              `difference` assignedInvestigatorIds
        player <- getPlayer iid
        push
          $ chooseOne
            player
            [ targetLabel
              iid'
              [ UseCardAbilityChoice
                  iid'
                  (EventSource eid)
                  2
                  (EncounterCardMetadata card)
                  []
                  NoPayment
              ]
            | iid' <- remainingInvestigatorIds
            ]
        pure e
      UseCardAbilityChoice iid (EventSource eid) 2 (EncounterCardMetadata card) _ _
        | eid == eventId ->
            pure
              $ FirstWatch
                ( attrs
                    `with` FirstWatchMetadata
                      { firstWatchPairings = (iid, card) : firstWatchPairings
                      }
                )
      UseCardAbilityChoice _ (EventSource eid) 3 (TargetMetadata _) _ _
        | eid == eventId ->
            e
              <$ pushAll
                [ InvestigatorDrewEncounterCard iid' card
                | (iid', card) <- firstWatchPairings
                ]
      RequestedEncounterCards (EventTarget eid) cards | eid == eventId -> do
        player <- getPlayer eventOwner
        pushAll
          [ chooseOneAtATime
              player
              [ TargetLabel
                (CardIdTarget $ toCardId card)
                [ UseCardAbilityChoice
                    eventOwner
                    (EventSource eventId)
                    1
                    (EncounterCardMetadata card)
                    []
                    NoPayment
                ]
              | card <- cards
              ]
          , UseCardAbilityChoice
              eventOwner
              (EventSource eventId)
              3
              (TargetMetadata $ toTarget attrs)
              []
              NoPayment
          ]
        pure e
      _ -> FirstWatch . (`with` metadata) <$> runMessage msg attrs
