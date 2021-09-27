module Arkham.Types.Enemy.Cards.WrithingAppendage
  ( writhingAppendage
  , WrithingAppendage(..)
  ) where

import Arkham.Prelude

import Arkham.Enemy.Cards qualified as Cards
import Arkham.Scenarios.CarnevaleOfHorrors.Helpers
import Arkham.Types.Ability
import Arkham.Types.Classes
import Arkham.Types.DamageEffect
import Arkham.Types.Enemy.Attrs
import Arkham.Types.Enemy.Helpers
import Arkham.Types.Enemy.Runner
import Arkham.Types.Matcher hiding (NonAttackDamageEffect)
import Arkham.Types.Message hiding (EnemyAttacks, EnemyDefeated)
import Arkham.Types.Timing qualified as Timing

newtype WrithingAppendage = WrithingAppendage EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor env)
  deriving newtype (Show, Eq, ToJSON, FromJSON, Entity)

writhingAppendage :: EnemyCard WrithingAppendage
writhingAppendage =
  enemy WrithingAppendage Cards.writhingAppendage (2, Static 2, 4) (1, 0)

instance HasAbilities WrithingAppendage where
  getAbilities (WrithingAppendage attrs) = withBaseAbilities
    attrs
    [ mkAbility attrs 1
    $ ForcedAbility
    $ EnemyAttacks Timing.After You
    $ EnemyWithId
    $ toId attrs
    , mkAbility attrs 2
    $ ForcedAbility
    $ EnemyDefeated Timing.When Anyone
    $ EnemyWithId
    $ toId attrs
    ]

instance EnemyRunner env => RunMessage env WrithingAppendage where
  runMessage msg e@(WrithingAppendage attrs) = case msg of
    UseCardAbility iid source _ 1 _ | isSource attrs source ->
      e <$ push (RandomDiscard iid)
    UseCardAbility iid source _ 2 _ | isSource attrs source -> do
      -- TODO: Damage here should not be dealt from an investigator to avoid
      -- triggering any abilities
      mCnidathquaId <- getCnidathqua
      case mCnidathquaId of
        Just cnidathquaId ->
          push
            (EnemyDamage
              cnidathquaId
              iid
              (toSource attrs)
              NonAttackDamageEffect
              1
            )
        Nothing -> pure ()
      WrithingAppendage <$> runMessage msg attrs
    _ -> WrithingAppendage <$> runMessage msg attrs
