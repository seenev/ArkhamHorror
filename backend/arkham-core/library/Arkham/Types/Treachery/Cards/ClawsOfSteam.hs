module Arkham.Types.Treachery.Cards.ClawsOfSteam
  ( clawsOfSteam
  , ClawsOfSteam(..)
  ) where

import Arkham.Prelude

import Arkham.Treachery.Cards qualified as Cards
import Arkham.Types.Classes
import Arkham.Types.Effect.Window
import Arkham.Types.EffectMetadata
import Arkham.Types.Game.Helpers
import Arkham.Types.Message
import Arkham.Types.Modifier
import Arkham.Types.SkillType
import Arkham.Types.Source
import Arkham.Types.Target
import Arkham.Types.Treachery.Attrs
import Arkham.Types.Treachery.Runner

newtype ClawsOfSteam = ClawsOfSteam TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor env, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

clawsOfSteam :: TreacheryCard ClawsOfSteam
clawsOfSteam = treachery ClawsOfSteam Cards.clawsOfSteam

instance TreacheryRunner env => RunMessage env ClawsOfSteam where
  runMessage msg t@(ClawsOfSteam attrs@TreacheryAttrs {..}) = case msg of
    Revelation iid source | isSource attrs source ->
      t <$ push (RevelationSkillTest iid source SkillWillpower 3)
    FailedSkillTest iid _ source SkillTestInitiatorTarget{} _ _
      | isSource attrs source -> t <$ pushAll
        [ CreateWindowModifierEffect
          EffectRoundWindow
          (EffectModifiers $ toModifiers attrs [CannotMove])
          source
          (InvestigatorTarget iid)
        , InvestigatorAssignDamage
          iid
          (TreacherySource treacheryId)
          DamageAssetsFirst
          2
          0
        ]
    _ -> ClawsOfSteam <$> runMessage msg attrs
