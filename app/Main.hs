module Main where

import Lib
import Control.Applicative
import Options

data MainOptions = MainOptions
    {
    -- optMessage :: String
    }

instance Options MainOptions where
    defineOptions = pure MainOptions
        -- <*> simpleOption "dry-run" "Do nothing, just print"
        -- "Something else"


mainWithOptions :: MainOptions -> [String] -> IO()
mainWithOptions opts targetFiles = runWithHosts (head targetFiles)

main :: IO ()
main = runCommand mainWithOptions
