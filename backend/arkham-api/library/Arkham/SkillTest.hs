{-# OPTIONS_GHC -Wno-orphans #-}

module Arkham.SkillTest (
  module X,
  module Arkham.SkillTest,
) where

import Arkham.Prelude

import Arkham.Card
import Arkham.ChaosToken
import Arkham.Helpers.SkillTest as X
import Arkham.Id
import Arkham.SkillTest.Base as X
import Arkham.SkillTest.Step
import Arkham.SkillTest.Type as X
import Arkham.SkillTestResult
import Arkham.SkillType
import Arkham.Target

iconValuesL :: Lens' SkillTest (Map SkillIcon Int)
iconValuesL = lens skillTestIconValues $ \m x -> m {skillTestIconValues = x}

skillTestTypeL :: Lens' SkillTest SkillTestType
skillTestTypeL = lens skillTestType $ \m x -> m {skillTestType = x}

targetCardL :: Lens' SkillTest (Maybe CardId)
targetCardL = lens skillTestTargetCard $ \m x -> m {skillTestTargetCard = x}

sourceCardL :: Lens' SkillTest (Maybe CardId)
sourceCardL = lens skillTestSourceCard $ \m x -> m {skillTestSourceCard = x}

difficultyL :: Lens' SkillTest SkillTestDifficulty
difficultyL = lens skillTestDifficulty $ \m x -> m {skillTestDifficulty = x}

subscribersL :: Lens' SkillTest [Target]
subscribersL = lens skillTestSubscribers $ \m x -> m {skillTestSubscribers = x}

setAsideChaosTokensL :: Lens' SkillTest [ChaosToken]
setAsideChaosTokensL = lens skillTestSetAsideChaosTokens $ \m x -> m {skillTestSetAsideChaosTokens = x}

resolvedChaosTokensL :: Lens' SkillTest [ChaosToken]
resolvedChaosTokensL = lens skillTestResolvedChaosTokens $ \m x -> m {skillTestResolvedChaosTokens = x}

toResolveChaosTokensL :: Lens' SkillTest [ChaosToken]
toResolveChaosTokensL = lens skillTestToResolveChaosTokens $ \m x -> m {skillTestToResolveChaosTokens = x}

revealedChaosTokensL :: Lens' SkillTest [ChaosToken]
revealedChaosTokensL = lens skillTestRevealedChaosTokens $ \m x -> m {skillTestRevealedChaosTokens = x}

committedCardsL :: Lens' SkillTest (Map InvestigatorId [Card])
committedCardsL = lens skillTestCommittedCards $ \m x -> m {skillTestCommittedCards = x}

resultL :: Lens' SkillTest SkillTestResult
resultL = lens skillTestResult $ \m x -> m {skillTestResult = x}

typeL :: Lens' SkillTest SkillTestType
typeL = lens skillTestType $ \m x -> m {skillTestType = x}

stepL :: Lens' SkillTest SkillTestStep
stepL = lens skillTestStep $ \m x -> m {skillTestStep = x}

baseValueL :: Lens' SkillTest SkillTestBaseValue
baseValueL = lens skillTestBaseValue $ \m x -> m {skillTestBaseValue = x}

resolveFailureInvestigatorL :: Lens' SkillTest InvestigatorId
resolveFailureInvestigatorL =
  lens skillTestResolveFailureInvestigator $ \m x -> m {skillTestResolveFailureInvestigator = x}
