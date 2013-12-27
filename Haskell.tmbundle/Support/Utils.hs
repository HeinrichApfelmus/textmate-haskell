{-----------------------------------------------------------------------------
    textmate-bundle
    
    Shared utility functions
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards #-}
module Utils where

import Control.Applicative
import Data.Char
import Data.List
import Data.Ord
import System.FilePath
import Text.ParserCombinators.ReadP as P

type Parser = P.ReadP

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


{-----------------------------------------------------------------------------
    import manipulations
------------------------------------------------------------------------------}
tidyImports :: String -> String
tidyImports = prettyImports . parseImports

data Import = Import
    { iQual    :: Bool
    , iModule  :: String
    , iAlias   :: Maybe String
    , iSymbols :: [String]
    } deriving (Eq, Ord, Show, Read)

-- pretty printing
prettyImports imports = unlines . map format . sortOn iModule $ imports
    where
    sortOn f = sortBy (comparing f)
    
    format (Import{..}) =
        "import"
        ++ (if iQual then " qualified " else qualspaces)
        ++ iModule ++ padding (length iModule)
        ++ maybe "" (" as " ++) iAlias
        ++ prettySymbols iSymbols
    
    padding n  = replicate (n - maxmodule) ' '
    maxmodule  = maximum $ map (length . iModule) imports
    qualspaces = if any iQual imports then map (const ' ') " qualified " else " "

prettySymbols [] = ""
prettySymbols xs = " (" ++ concat (intersperse ", " xs) ++ " )"

-- parsing
parseImports  = fst . last . readP_to_S pImports

pImports :: Parser [Import]
pImports = skipSpaces *> manyTill pImport ((() <$ satisfy ('i' /=)) <++ eof)

pImport = Import <$ string "import" <* skipSpaces
    <*> pQual    <* skipSpaces
    <*> pModule  <* skipSpaces
    <*> pAlias   <* skipSpaces
    <*> pSymbols <* skipSpaces

pQual    = option False (True <$ string "qualified") <* skipSpaces
pModule  = munch1 $ \c -> c == '.' || isAlpha c
pAlias   = leftoption Nothing (Just <$> (string "as" *> skipSpaces *> pModule))
pSymbols = leftoption [] $ string "(" *>    
    sepBy pSymbol (skipSpaces *> string "," *> skipSpaces)
    <* string ")"
pSymbol  = munch1 $ \c -> isAlpha c || c == '\''

leftoption a p = p <++ return a




