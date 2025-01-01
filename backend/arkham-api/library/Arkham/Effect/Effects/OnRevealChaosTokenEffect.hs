module Arkham.Effect.Effects.OnRevealChaosTokenEffect (
  OnRevealChaosTokenEffect (..),
  onRevealChaosTokenEffect,
  onRevealChaosTokenEffect',
) where

import Arkham.Classes
import Arkham.Effect.Runner
import Arkham.Game.Helpers (chaosTokenMatches)
import Arkham.Id
import Arkham.Matcher hiding (RevealChaosToken)
import Arkham.Prelude
import Arkham.Window qualified as Window

newtype OnRevealChaosTokenEffect = OnRevealChaosTokenEffect EffectAttrs
  deriving anyclass (HasAbilities, IsEffect)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

onRevealChaosTokenEffect :: EffectArgs -> OnRevealChaosTokenEffect
onRevealChaosTokenEffect = OnRevealChaosTokenEffect . uncurry (baseAttrs "ontok")

onRevealChaosTokenEffect'
  :: EffectId
  -> SkillTestId
  -> ChaosTokenMatcher
  -> Source
  -> Target
  -> [Message]
  -> OnRevealChaosTokenEffect
onRevealChaosTokenEffect' eid skillTestId matchr source target msgs =
  OnRevealChaosTokenEffect
    $ EffectAttrs
      { effectId = eid
      , effectSource = source
      , effectTarget = target
      , effectCardCode = "ontok"
      , effectMetadata = Just (EffectMessages msgs)
      , effectTraits = mempty
      , effectWindow = Nothing
      , effectFinished = False
      , effectExtraMetadata = toJSON matchr
      , effectSkillTest = Just skillTestId
      , effectCardId = Nothing
      }

instance HasModifiersFor OnRevealChaosTokenEffect

instance RunMessage OnRevealChaosTokenEffect where
  runMessage msg e@(OnRevealChaosTokenEffect attrs) = runQueueT $ case msg of
    RevealChaosToken _ iid token -> do
      void $ runMaybeT do
        matchr <- hoistMaybe $ maybeResult $ effectExtraMetadata attrs
        liftGuardM $ chaosTokenMatches token matchr
        sid <- MaybeT getSkillTestId
        guard $ maybe False (== sid) (effectSkillTest attrs)
        case attrs.metadata of
          Just (EffectMessages msgs) -> lift do
            push $ DisableEffect attrs.id
            case attrs.source of
              EventSource eid -> push $ If (Window.RevealChaosTokenEventEffect iid [token] eid) msgs
              AbilitySource inner _n -> case inner of
                AssetSource aid -> push $ If (Window.RevealChaosTokenAssetAbilityEffect iid [token] aid) msgs
                other -> error $ "Unhandled ability source for token effect: " <> show other
              AssetSource aid -> push $ If (Window.RevealChaosTokenAssetAbilityEffect iid [token] aid) msgs
              TreacherySource tid -> push $ If (Window.RevealChaosTokenTreacheryEffect iid [token] tid) msgs
              other -> error $ "Unhandled source for token effect: " <> show other
          _ -> pure ()
      pure e
    SkillTestEnds _ _ _ -> do
      push $ DisableEffect attrs.id
      pure e
    _ -> OnRevealChaosTokenEffect <$> liftRunMessage msg attrs
