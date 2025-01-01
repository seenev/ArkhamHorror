module Arkham.Event.Events.OccultEvidence (occultEvidence, OccultEvidence (..)) where

import Arkham.Ability
import Arkham.Card
import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Discover
import Arkham.Event.Cards qualified as Cards
import Arkham.Event.Runner
import Arkham.Helpers.Investigator
import Arkham.Investigator.Types (Field (..))
import Arkham.Location.Types (Field (..))
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Projection
import Arkham.Trait (Trait (Research))

newtype OccultEvidence = OccultEvidence EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultEvidence :: EventCard OccultEvidence
occultEvidence = event OccultEvidence Cards.occultEvidence

instance HasAbilities OccultEvidence where
  getAbilities (OccultEvidence x) =
    [ playerLimit (PerSearch Research)
        $ mkAbility x 1
        $ ReactionAbility (AmongSearchedCards You) (RevealCost $ toCardId x)
    ]

instance RunMessage OccultEvidence where
  runMessage msg e@(OccultEvidence attrs) = case msg of
    InSearch (UseThisAbility iid (isSource attrs -> True) 1) -> do
      mLocation <- field InvestigatorLocation iid
      canDiscoverClues <- maybe (pure False) (getCanDiscoverClues NotInvestigate iid) mLocation
      hasClues <- maybe (pure False) (fieldSome LocationClues) mLocation
      let source = toAbilitySource attrs 1
      pushAll
        $ [RemoveCardFromSearch iid (toCardId attrs), DrawToHandFrom iid (Deck.toDeck iid) [toCard attrs]]
        <> [ Msg.DiscoverClues iid $ discover lid source 1 | canDiscoverClues && hasClues, lid <- toList mLocation
           ]
      pure e
    PlayThisEvent iid eid | eid == toId attrs -> do
      push $ ShuffleIntoDeck (Deck.InvestigatorDeck iid) (toTarget attrs)
      pure e
    _ -> OccultEvidence <$> runMessage msg attrs
