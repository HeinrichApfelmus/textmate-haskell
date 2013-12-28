{-----------------------------------------------------------------------------
    textmate-bundle
    
    Shared utility functions
------------------------------------------------------------------------------}
module Utils where

import Data.List
import System.FilePath

{-----------------------------------------------------------------------------
    file path manipulations
------------------------------------------------------------------------------}
-- | List all parent directories.
-- The idea is to use them as search paths for GHCi.
parentDirs :: FilePath -> [FilePath]
parentDirs = map joinPath . tail . inits . init . splitPath

-- | Generate command line arguments to include parent directories
includes :: FilePath -> [String]
includes = map ("-i"++) . parentDirs




