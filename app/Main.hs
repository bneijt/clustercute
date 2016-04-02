module Main where

import Lib
import Options.Applicative

data ClusterCuteOptions = ClusterCuteOptions
  { hostFilePath :: FilePath }

clusterCuteOptions :: Parser ClusterCuteOptions
clusterCuteOptions = ClusterCuteOptions
     <$> argument str (metavar "HOST-COMMAND-FILE")

mainWithOptions :: ClusterCuteOptions -> IO ()
mainWithOptions (ClusterCuteOptions hostFilePath) = runTargets hostFilePath

main :: IO ()
main = execParser opts >>= mainWithOptions
  where
    opts = info (helper <*> clusterCuteOptions)
      ( fullDesc
     <> progDesc "SSH into hosts metioned in HOST-COMMAND-FILE"
     <> header "Clustercute - Simple SSH client command executor based on password logins" )
