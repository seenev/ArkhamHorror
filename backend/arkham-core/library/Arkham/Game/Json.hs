{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.Game.Json where

import Arkham.Game.Base
import Arkham.Prelude
import Data.Aeson.TH

-- bring json instances into scope

import Arkham.Campaign ()
import Arkham.Entities ()
import Arkham.Investigator ()
import Arkham.Scenario ()

$(deriveJSON defaultOptions ''GameState)
$(deriveJSON (defaultOptions {allowOmittedFields = True}) ''Game)
