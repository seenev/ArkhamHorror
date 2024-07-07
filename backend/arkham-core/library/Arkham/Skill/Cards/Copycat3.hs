module Arkham.Skill.Cards.Copycat3 (copycat3, copycat3Effect, Copycat3 (..)) where

import Arkham.Classes
import Arkham.Deck qualified as Deck
import Arkham.Effect.Runner ()
import Arkham.Effect.Types
import Arkham.EffectMetadata
import {-# SOURCE #-} Arkham.GameEnv
import Arkham.Matcher
import Arkham.Message
import Arkham.Prelude
import Arkham.Skill.Cards qualified as Cards
import Arkham.Skill.Runner

newtype Copycat3 = Copycat3 SkillAttrs
  deriving anyclass (IsSkill, HasModifiersFor, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

copycat3 :: SkillCard Copycat3
copycat3 = skill Copycat3 Cards.copycat3

instance RunMessage Copycat3 where
  runMessage msg (Copycat3 attrs) = case msg of
    InvestigatorCommittedSkill iid sid | sid == toId attrs -> do
      iids <- select $ not_ $ InvestigatorWithId iid
      iidsWithCommittableCards <- forMaybeM iids $ \iid' -> do
        committableCards <- select $ CommittableCard iid $ inDiscardOf iid' <> #skill
        pure $ guard (null committableCards) $> (iid', committableCards)
      player <- getPlayer iid
      unless (null iidsWithCommittableCards)
        $ pushAll
          [ FocusCards (concatMap snd iidsWithCommittableCards)
          , chooseOne
              player
              [ targetLabel
                card
                [ CommitCard iid card
                , createCardEffect Cards.copycat3 (Just $ EffectMetaTarget (toTarget card)) attrs iid'
                ]
              | (iid', cards) <- iidsWithCommittableCards
              , card <- cards
              ]
          , UnfocusCards
          ]
      Copycat3 <$> runMessage msg attrs
    _ -> Copycat3 <$> runMessage msg attrs

newtype Copycat3Effect = Copycat3Effect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

copycat3Effect :: EffectArgs -> Copycat3Effect
copycat3Effect = cardEffect Copycat3Effect Cards.copycat3

instance RunMessage Copycat3Effect where
  runMessage msg e@(Copycat3Effect attrs@EffectAttrs {..}) = case msg of
    SkillTestEnds _ _ -> do
      case (effectMetadata, effectTarget) of
        (Just (EffectMetaTarget (CardIdTarget cardId)), InvestigatorTarget iid) ->
          do
            card <- getCard cardId
            pushAll
              [ DisableEffect effectId
              , PutCardOnBottomOfDeck iid (Deck.InvestigatorDeck iid) card
              ]
        _ -> error "invalid target or effectMetaTarget"
      pure e
    _ -> Copycat3Effect <$> runMessage msg attrs
