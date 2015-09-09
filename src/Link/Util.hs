module Link.Util where

import System.IO   (Handle)
import Text.Printf (hPrintf)

printToHandle :: Handle -> String -> IO ()
printToHandle handle = hPrintf handle "%s\n"
