module Arkham.Investigator.Cards.RexMurphy (RexMurphy (..), rexMurphy) where

import Arkham.Ability
import Arkham.Discover
import Arkham.Investigator.Cards qualified as Cards
import Arkham.Investigator.Runner
import Arkham.Matcher
import Arkham.Message qualified as Msg
import Arkham.Prelude
import Arkham.Taboo

newtype RexMurphy = RexMurphy InvestigatorAttrs
  deriving anyclass (IsInvestigator, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)
  deriving stock Data

rexMurphy :: InvestigatorCard RexMurphy
rexMurphy =
  investigator RexMurphy Cards.rexMurphy
    $ Stats {health = 6, sanity = 9, willpower = 3, intellect = 4, combat = 2, agility = 3}

instance HasAbilities RexMurphy where
  getAbilities (RexMurphy x) =
    [ (if (maybe False (>= TabooList15) x.taboo) then playerLimit PerRound else id)
        $ (restrictedAbility x 1)
          (OnLocation LocationWithAnyClues <> CanDiscoverCluesAt YourLocation)
          (freeReaction $ SuccessfulInvestigationResult #after You Anywhere (atLeast 2))
    ]

instance HasChaosTokenValue RexMurphy where
  getChaosTokenValue iid ElderSign (RexMurphy attrs) | iid == toId attrs = do
    pure $ ChaosTokenValue ElderSign (PositiveModifier 2)
  getChaosTokenValue _ token _ = pure $ ChaosTokenValue token mempty

instance RunMessage RexMurphy where
  runMessage msg i@(RexMurphy attrs) = case msg of
    UseThisAbility iid (isSource attrs -> True) 1 -> do
      push $ Msg.DiscoverClues iid $ discoverAtYourLocation (toAbilitySource attrs 1) 1
      pure i
    ResolveChaosToken _drawnToken ElderSign iid | iid == toId attrs -> do
      player <- getPlayer iid
      push
        $ chooseOne player
        $ [ Label
              "Automatically fail to draw 3"
              [FailSkillTest, drawCards iid (ChaosTokenEffectSource ElderSign) 3]
          , Label "Resolve normally" []
          ]
      pure i
    _ -> RexMurphy <$> runMessage msg attrs
