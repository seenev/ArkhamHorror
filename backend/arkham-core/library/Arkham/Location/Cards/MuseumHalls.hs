module Arkham.Location.Cards.MuseumHalls (museumHalls, MuseumHalls (..)) where

import Arkham.Ability
import Arkham.Classes
import Arkham.Draw.Types
import Arkham.GameValue
import Arkham.Location.Cards qualified as Cards (museumHalls)
import Arkham.Location.Helpers
import Arkham.Location.Runner
import Arkham.Matcher
import Arkham.Prelude
import Arkham.Scenario.Deck

newtype MuseumHalls = MuseumHalls LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

museumHalls :: LocationCard MuseumHalls
museumHalls =
  locationWith MuseumHalls Cards.museumHalls 2 (Static 0)
    $ revealedConnectedMatchersL
    <>~ ["Exhibit Hall"]

instance HasModifiersFor MuseumHalls where
  getModifiersFor target (MuseumHalls l) | l `is` target = do
    pure $ toModifiers l [Blocked | unrevealed l]
  getModifiersFor _ _ = pure []

instance HasAbilities MuseumHalls where
  getAbilities (MuseumHalls attrs) | unrevealed attrs = do
    withBaseAbilities
      attrs
      [ withTooltip
          "{action}: Test {combat} (5) to attempt to break down the door to the museum. If you are successful, immediately advance to Act 1b"
          $ restrictedAbility
            (proxied (LocationMatcherSource "Museum Entrance") attrs)
            1
            (OnLocation "Museum Entrance")
            #action
      ]
  getAbilities (MuseumHalls attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here $ actionAbilityWithCost $ GroupClueCost (PerPlayer 1) "Museum Halls"
        ]

instance RunMessage MuseumHalls where
  runMessage msg l@(MuseumHalls attrs) = case msg of
    UseThisAbility iid this@(isProxySource attrs -> True) 1 | unrevealed attrs -> do
      museumEntrance <- selectJust $ LocationWithTitle "Museum Entrance"
      sid <- getRandom
      push $ beginSkillTest sid iid (toAbilitySource this 1) museumEntrance #combat (Fixed 5)
      pure l
    UseThisAbility iid (isSource attrs -> True) 1 | revealed attrs -> do
      push $ DrawCards iid $ targetCardDraw attrs ExhibitDeck 1
      pure l
    DrewCards _ drewCards | maybe False (isTarget attrs) drewCards.target -> do
      placements <- traverse placeLocation_ drewCards.cards
      pushAll placements
      pure l
    PassedThisSkillTest _ source@(isProxyAbilitySource attrs 1 -> True) -> do
      actId <- selectJust AnyAct
      push $ AdvanceAct actId source AdvancedWithOther
      pure l
    _ -> MuseumHalls <$> runMessage msg attrs
