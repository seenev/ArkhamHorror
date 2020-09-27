{-# LANGUAGE UndecidableInstances #-}
module Arkham.Types.Investigator.Cards.DianaStanley where

import Arkham.Types.Classes
import Arkham.Types.ClassSymbol
import Arkham.Types.Investigator.Attrs
import Arkham.Types.Investigator.Runner
import Arkham.Types.Message
import Arkham.Types.Stats
import qualified Arkham.Types.Token as Token
import Arkham.Types.Trait
import ClassyPrelude
import Data.Aeson

newtype DianaStanley = DianaStanley Attrs
  deriving newtype (Show, ToJSON, FromJSON)

dianaStanley :: DianaStanley
dianaStanley = DianaStanley $ baseAttrs
  "05004"
  "Diana Stanley"
  Mystic
  Stats
    { health = 7
    , sanity = 7
    , willpower = 1
    , intellect = 3
    , combat = 3
    , agility = 3
    }
  [Cultist, SilverTwilight]

instance HasActions env investigator DianaStanley where
  getActions i window (DianaStanley attrs) = getActions i window attrs

instance (InvestigatorRunner Attrs env) => RunMessage env DianaStanley where
  runMessage msg i@(DianaStanley attrs@Attrs {..}) = case msg of
    ResolveToken Token.ElderSign iid | iid == investigatorId ->
      pure i
    _ -> DianaStanley <$> runMessage msg attrs
