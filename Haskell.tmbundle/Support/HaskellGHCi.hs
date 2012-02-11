{-----------------------------------------------------------------------------
    textmate-bundle
    
    Smart GHCI
------------------------------------------------------------------------------}
import System.Cmd
import System.Environment

import Utils (includes)

main = do
    [path] <- getArgs
    rawSystem "ghci" $ includes path ++ [path]