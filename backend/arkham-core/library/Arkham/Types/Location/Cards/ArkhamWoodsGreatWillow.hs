module Arkham.Types.Location.Cards.ArkhamWoodsGreatWillow where

import Arkham.Prelude

import Arkham.Location.Cards qualified as Cards (arkhamWoodsGreatWillow)
import Arkham.Types.Ability
import Arkham.Types.Card.CardCode
import Arkham.Types.Classes
import Arkham.Types.Criteria
import Arkham.Types.Game.Helpers
import Arkham.Types.GameValue
import Arkham.Types.Location.Attrs
import Arkham.Types.Matcher
import Arkham.Types.Message
import Arkham.Types.SkillTest
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Timing qualified as Timing

newtype ArkhamWoodsGreatWillow = ArkhamWoodsGreatWillow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

arkhamWoodsGreatWillow :: LocationCard ArkhamWoodsGreatWillow
arkhamWoodsGreatWillow = locationWithRevealedSideConnections
  ArkhamWoodsGreatWillow
  Cards.arkhamWoodsGreatWillow
  4
  (PerPlayer 1)
  Square
  [Squiggle]
  Heart
  [Squiggle, Star]

instance HasAbilities ArkhamWoodsGreatWillow where
  getAbilities (ArkhamWoodsGreatWillow attrs) =
    withBaseAbilities attrs
      $ [ restrictedAbility attrs 1 Here
          $ ForcedAbility
          $ SkillTestResult Timing.After You (SkillTestOnTreachery AnyTreachery)
          $ SuccessResult AnyValue
        | locationRevealed attrs
        ]

instance LocationRunner env => RunMessage env ArkhamWoodsGreatWillow where
  runMessage msg l@(ArkhamWoodsGreatWillow attrs) = case msg of
    UseCardAbility _ source _ 1 _ | isSource attrs source ->
      getSkillTestSource >>= \case
        Just (TreacherySource tid) ->
          l
            <$ push
                 (CreateEffect
                   (toCardCode attrs)
                   Nothing
                   source
                   (TreacheryTarget tid)
                 )
        _ -> error "Invalid use if Arkham Woods: Great Willow ability"
    _ -> ArkhamWoodsGreatWillow <$> runMessage msg attrs
