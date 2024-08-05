module Arkham.Asset.Cards.Augur (augur, Augur (..)) where

import Arkham.Ability
import Arkham.Asset.Cards qualified as Cards
import Arkham.Asset.Runner
import Arkham.Card
import Arkham.Deck qualified as Deck
import Arkham.Investigate
import Arkham.Investigator.Types (Field (..))
import Arkham.Matcher hiding (AssetCard)
import Arkham.Prelude
import Arkham.Projection

newtype Augur = Augur AssetAttrs
  deriving anyclass (IsAsset, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

augur :: AssetCard Augur
augur = asset Augur Cards.augur

instance HasAbilities Augur where
  getAbilities (Augur a) =
    [ controlledAbility a 1 (exists $ oneOf [assetIs Cards.zeal, assetIs Cards.hope])
        $ forced
        $ AssetEntersPlay #when
        $ be a
    , controlledAbility a 2 (exists $ AssetWithId (toId a) <> AssetReady)
        $ investigateAction
        $ OrCost [exhaust a, discardCost a]
    ]

instance RunMessage Augur where
  runMessage msg a@(Augur attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      otherCats <- select $ oneOf [assetIs Cards.zeal, assetIs Cards.hope]
      for_ otherCats $ push . toDiscardBy iid (toAbilitySource attrs 1)
      pure a
    UseThisAbility iid (isSource attrs -> True) 2 -> do
      let source = toAbilitySource attrs 2
      discarded <- selectNone $ AssetWithId (toId attrs)
      catsInDiscard <-
        fieldMap
          InvestigatorDiscard
          (filter (`cardMatch` oneOf [cardIs Cards.zeal, cardIs Cards.hope]))
          iid
      player <- getPlayer iid
      augurCard <- field AssetCard (toId attrs)
      sid <- getRandom
      investigation <- mkInvestigate sid iid source
      pushAll
        $ [skillTestModifier sid source iid (BaseSkillOf #intellect 5)]
        <> [skillTestModifier sid source iid SkillTestAutomaticallySucceeds | discarded]
        <> [toMessage investigation]
        <> [ questionLabel "Put into play from discard" player
            $ ChooseOne
            $ [ CardLabel
                (toCardCode card)
                [ ShuffleCardsIntoDeck (Deck.InvestigatorDeck iid) [augurCard]
                , PutCardIntoPlay iid (toCard card) Nothing NoPayment []
                ]
              | card <- catsInDiscard
              ]
            <> [Label "Skip" []]
           | notNull catsInDiscard
           ]
      pure a
    _ -> Augur <$> runMessage msg attrs
