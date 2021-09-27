module Arkham.Types.Effect.Effects.Deduction2
  ( deduction2
  , Deduction2(..)
  ) where

import Arkham.Prelude

import Arkham.Types.Action qualified as Action
import Arkham.Types.Classes
import Arkham.Types.Effect.Attrs
import Arkham.Types.EffectMetadata
import Arkham.Types.Message
import Arkham.Types.Target

newtype Deduction2 = Deduction2 EffectAttrs
  deriving anyclass HasAbilities
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

deduction2 :: EffectArgs -> Deduction2
deduction2 = Deduction2 . uncurry4 (baseAttrs "02150")

instance HasModifiersFor env Deduction2

instance HasQueue env => RunMessage env Deduction2 where
  runMessage msg e@(Deduction2 attrs@EffectAttrs {..}) = case msg of
    Successful (Action.Investigate, _) iid _ (LocationTarget lid) ->
      case effectMetadata of
        Just (EffectMetaTarget (LocationTarget lid')) | lid == lid' ->
          e <$ push
            (InvestigatorDiscoverClues iid lid 1 (Just Action.Investigate))
        _ -> pure e
    SkillTestEnds _ -> e <$ push (DisableEffect effectId)
    _ -> Deduction2 <$> runMessage msg attrs
