{-# LANGUAGE RecordWildCards #-}

module SetupHooks where

import Control.Monad
import Data.List.Extra
import Data.String
import Distribution.ModuleName
import Distribution.Simple.SetupHooks
import Distribution.Utils.Path
import System.FilePath
import System.FilePattern.Directory
import qualified Data.Set as Set


setupHooks :: SetupHooks
setupHooks =
 noSetupHooks
   { configureHooks = myConfigureHooks
   }

myConfigureHooks :: ConfigureHooks
myConfigureHooks = noConfigureHooks
  { preConfComponentHook = Just $ \PreConfComponentInputs{..} -> case component of
      CLib (lib@Library { libName = LMainLibName, libBuildInfo = bi }) -> do
        let sourceDirs = interpretSymbolicPath Nothing <$> hsSourceDirs bi
        moduleFiles <- fmap mconcat $ forM sourceDirs getHaskellFiles
        let existingModules = Set.fromList $ exposedModules lib
            discoveredModules = Set.fromList
                                  $ filePathToModuleName
                                 <$> moduleFiles
            distinctModules = discoveredModules `Set.difference` existingModules
        pure
          $ PreConfComponentOutputs
              (ComponentDiff
                (CLib lib{ exposedModules = Set.toList distinctModules }))
      lib -> do
        pure (PreConfComponentOutputs (ComponentDiff component))
  }
 where
  filePathToModuleName :: FilePath -> ModuleName
  filePathToModuleName = fromString . replace "/" "." . dropExtension

  getHaskellFiles :: FilePath -> IO [FilePath]
  getHaskellFiles sourceDir = getDirectoryFiles sourceDir ["**/*.hs","**/*.hsc"]

