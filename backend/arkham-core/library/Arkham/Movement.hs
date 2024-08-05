{-# LANGUAGE TemplateHaskell #-}

module Arkham.Movement where

import Arkham.Id
import Arkham.Matcher
import {-# SOURCE #-} Arkham.Message (Message)
import Arkham.Prelude
import Arkham.Source
import Arkham.Target
import Data.Aeson.TH

data Movement = Movement
  { moveSource :: Source
  , moveTarget :: Target
  , moveDestination :: Destination
  , moveMeans :: MovementMeans
  , moveCancelable :: Bool
  , movePayAdditionalCosts :: Bool
  , moveAfter :: [Message]
  }
  deriving stock (Show, Eq, Data)

data MovementMeans = Direct | OneAtATime | Towards
  deriving stock (Show, Eq, Data)

uncancellableMove :: Movement -> Movement
uncancellableMove m = m {moveCancelable = False}

afterMove :: [Message] -> Movement -> Movement
afterMove msgs m = m {moveAfter = msgs}

data Destination = ToLocation LocationId | ToLocationMatching LocationMatcher
  deriving stock (Show, Eq, Data)

move
  :: (Targetable target, Sourceable source)
  => source
  -> target
  -> LocationId
  -> Movement
move (toSource -> moveSource) (toTarget -> moveTarget) lid =
  Movement
    { moveSource
    , moveTarget
    , moveDestination = ToLocation lid
    , moveMeans = Direct
    , moveCancelable = True
    , movePayAdditionalCosts = True
    , moveAfter = []
    }

moveToMatch
  :: (Targetable target, Sourceable source)
  => source
  -> target
  -> LocationMatcher
  -> Movement
moveToMatch (toSource -> moveSource) (toTarget -> moveTarget) matcher =
  Movement
    { moveSource
    , moveTarget
    , moveDestination = ToLocationMatching matcher
    , moveMeans = Direct
    , moveCancelable = True
    , movePayAdditionalCosts = True
    , moveAfter = []
    }

moveTowardsMatching
  :: (Targetable target, Sourceable source)
  => source
  -> target
  -> LocationMatcher
  -> Movement
moveTowardsMatching (toSource -> moveSource) (toTarget -> moveTarget) matcher =
  Movement
    { moveSource
    , moveTarget
    , moveDestination = ToLocationMatching matcher
    , moveMeans = Towards
    , moveCancelable = True
    , movePayAdditionalCosts = True
    , moveAfter = []
    }

moveToLocationMatcher :: Movement -> LocationMatcher
moveToLocationMatcher = destinationToLocationMatcher . moveDestination

destinationToLocationMatcher :: Destination -> LocationMatcher
destinationToLocationMatcher = \case
  ToLocation lid -> LocationWithId lid
  ToLocationMatching matcher -> matcher

$(deriveJSON defaultOptions ''MovementMeans)
$(deriveJSON defaultOptions ''Destination)
$(deriveJSON defaultOptions ''Movement)
