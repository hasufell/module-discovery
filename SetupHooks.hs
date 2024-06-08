{-# LANGUAGE RecordWildCards #-}

module SetupHooks where

import Control.Monad
import Data.List.Extra
import Data.String
import Distribution.Simple.SetupHooks
import Distribution.Utils.Path
import System.FilePath
import System.FilePattern.Directory


setupHooks :: SetupHooks
setupHooks =
 noSetupHooks
   { configureHooks = myConfigureHooks
   }

myConfigureHooks :: ConfigureHooks
myConfigureHooks =
 noConfigureHooks
   { preConfComponentHook = Just $ \PreConfComponentInputs{..} ->
       case component of
         CLib (lib@Library { libName = LMainLibName, libBuildInfo = bi }) -> do
           let dirs = interpretSymbolicPath Nothing <$> hsSourceDirs bi
           moduleFiles <- fmap mconcat $ forM dirs $ \dir -> getDirectoryFiles dir ["**/*.hs","**/*.hsc"]
           let modules = fromString . replace "/" "." . dropExtension <$> moduleFiles
           pure (PreConfComponentOutputs (ComponentDiff (CLib lib{ exposedModules = modules })))
         lib -> do
           pure (PreConfComponentOutputs (ComponentDiff component))
   }

