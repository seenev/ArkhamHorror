module Arkham.Types.Asset.Cards.OccultLexicon where

import Arkham.Prelude

import Arkham.Asset.Cards qualified as Cards
import Arkham.Event.Cards qualified as Events
import Arkham.Types.Asset.Attrs
import Arkham.Types.Asset.Runner
import Arkham.Types.Card
import Arkham.Types.Card.PlayerCard
import Arkham.Types.Classes
import Arkham.Types.Message

newtype OccultLexicon = OccultLexicon AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

occultLexicon :: AssetCard OccultLexicon
occultLexicon = hand OccultLexicon Cards.occultLexicon

instance AssetRunner env => RunMessage env OccultLexicon where
  runMessage msg (OccultLexicon attrs) = case msg of
    InvestigatorPlayAsset iid aid _ _ | aid == assetId attrs -> do
      handBloodRite <- PlayerCard <$> genPlayerCard Events.bloodRite
      deckBloodRites <- replicateM 2 (genPlayerCard Events.bloodRite)
      pushAll
        [AddToHand iid handBloodRite, ShuffleCardsIntoDeck iid deckBloodRites]
      OccultLexicon <$> runMessage msg attrs
    RemovedFromPlay source | isSource attrs source -> do
      for_ (assetInvestigator attrs)
        $ \iid -> push (RemoveAllCopiesOfCardFromGame iid "05317")
      OccultLexicon <$> runMessage msg attrs
    _ -> OccultLexicon <$> runMessage msg attrs
