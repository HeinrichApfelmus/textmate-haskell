{-----------------------------------------------------------------------------
    textmate-bundle
------------------------------------------------------------------------------}
{-# LANGUAGE RecordWildCards #-}
module Imports where

import Control.Applicative
import Data.Char
import Data.List
import Data.Ord
import Text.ParserCombinators.ReadP as P

import Utils

type Parser = P.ReadP

{-----------------------------------------------------------------------------
    Import manipulations
------------------------------------------------------------------------------}
tidyImports :: String -> String
tidyImports = prettyImports . parseImports

data Import = Import
    { iQual    :: Bool
    , iModule  :: String
    , iAlias   :: Maybe String
    , iHiding  :: Bool
    , iSymbols :: [String]
    } deriving (Eq, Ord, Show, Read)

-- pretty printing
prettyImports imports = unlines' . map format . sortOn iModule $ imports
    where
    sortOn f = sortBy (comparing f)
    
    format (Import{..}) =
        "import"
        ++ (if iQual then " qualified " else qualspaces)
        ++ iModule ++ padding maxmodule (length iModule)
        ++ maybe "" (" as " ++) iAlias
        ++ padding maxalias (maybe 0 length iAlias)
        ++ prettySymbols (iHiding, iSymbols)
    
    padding max n = replicate (max - n) ' '
    maxalias      = maximum $ map (maybe 0 id . fmap (length . (" as " ++)) . iAlias) imports 
    maxmodule     = maximum $ map (length . iModule) imports
    qualspaces    = if any iQual imports then map (const ' ') " qualified " else " "

prettySymbols (hiding, []) = ""
prettySymbols (hiding, xs) =
    (if hiding then " hiding" else "")
    ++ " (" ++ concat (intersperse ", " xs) ++ ")"


-- parsing
parseImports  = fst . last . readP_to_S pImports

-- TODO: use some sort of a lexer
pImports :: Parser [Import]
pImports = skipSpaces *> manyTill pImport ((() <$ satisfy ('i' /=)) <++ eof)

pImport = Import <$ string "import" <* skipSpaces
    <*> pQual    <* skipSpaces
    <*> pModule  <* skipSpaces
    <*> pAlias   <* skipSpaces
    <*> pHiding  <* skipSpaces
    <*> pSymbols <* skipSpaces

pQual    = option False (True <$ string "qualified")
pHiding  = option False (True <$ string "hiding"   )
pModule  = munch1 $ \c -> c == '.' || isAlpha c
pAlias   = leftoption Nothing (Just <$> (string "as" *> skipSpaces *> pModule))
-- FIXME: Constructor field T(..) are not parsed correctly.
pSymbols = leftoption [] $ string "(" *> skipSpaces *>
    sepBy pSymbol (skipSpaces *> string "," *> skipSpaces)
    <* skipSpaces <* string ")"
pSymbol  = munch1 $ \c -> isAlpha c || c == '\''

leftoption a p = p <++ return a


