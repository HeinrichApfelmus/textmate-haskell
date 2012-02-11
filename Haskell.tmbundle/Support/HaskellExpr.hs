{-----------------------------------------------------------------------------
    textmate-bundle
    
    Evaluate the current Haskell expression in GHC
------------------------------------------------------------------------------}
import Data.Char
import Data.Maybe (listToMaybe)
import Data.List (isPrefixOf, inits)
import System.Cmd
import System.Environment

import Utils (includes)

main = do
    lineNo <- getEnv "TM_LINE_NUMBER"
    dir    <- getEnv "TM_DIRECTORY"
    path   <- getEnv "TM_FILEPATH"
    [mode] <- getArgs
    
    s <- readFile path
    let mexpr = findTopLevelDefinition (read lineNo) s
    case mexpr of
        Nothing -> do
            putStr "(No top-level expression found to evaluate.)"
        Just expr -> do
            let command  = if mode == "demo" then "demo " ++ expr else expr
            rawSystem "ghc" $ ["-e",command,path] ++ includes path
            return ()


-- | Find the top level definition that the current line is part of.
-- Not very sophisticated, we're just looking for indentation.
findTopLevelDefinition :: Int -> String -> Maybe String
findTopLevelDefinition lineNo document
    = filterMaybe (\x -> all isAlphaNum x && x /= "import" )
    . (listToMaybe . words =<<) . listToMaybe
    . filter (not . (" " `isPrefixOf`)) . reverse 
    . filter (not . null) . take lineNo . lines
    $ document
    where
    filterMaybe p mx = mx >>= \x -> if p x then Just x else Nothing
    

