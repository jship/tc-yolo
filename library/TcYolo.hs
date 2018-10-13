module TcYolo
  ( plugin
  ) where

import Class (className)
import Control.Monad (guard)
import Data.Maybe (mapMaybe)
import OccName (HasOccName(occName), mkClsOcc)
import Plugins (Plugin, defaultPlugin, tcPlugin)
import TcEvidence (EvTerm)
import TcRnTypes
  ( TcPlugin(TcPlugin, tcPluginInit, tcPluginSolve, tcPluginStop), TcPluginResult(TcPluginOk), Ct
  , ctEvPred, ctEvTerm, ctEvidence
  )
import Type (PredTree(ClassPred), classifyPredType)

plugin :: Plugin
plugin = defaultPlugin
  { tcPlugin = const . pure $ TcPlugin
    { tcPluginInit = pure ()
    , tcPluginStop = const (pure ())
    , tcPluginSolve = \() _givenCts _derivedCts wantedCts ->
        pure . TcPluginOk (mapMaybe solveMonadIOCt wantedCts) $ []
    }
  }

solveMonadIOCt :: Ct -> Maybe (EvTerm, Ct)
solveMonadIOCt ct = do
  ClassPred cls _types <- pure . classifyPredType . ctEvPred . ctEvidence $ ct
  guard (mkClsOcc "MonadIO" == occName (className cls))
  -- The first part of this pair is probably wrong? ¯\_(ツ)_/¯
  pure (ctEvTerm . ctEvidence $ ct, ct)
