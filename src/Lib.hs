module Lib
    ( runTargets
    ) where

import System.IO
import System.Exit (die)
import Network.SSH.Client.LibSSH2
import Network.SSH.Client.LibSSH2.Foreign (initialize, exit)
import Control.Concurrent.ParallelIO.Global
import System.Log.Logger (infoM, Priority (INFO, DEBUG), setLevel, updateGlobalLogger, addHandler)
import System.Log.Handler.Simple (fileHandler)
import System.Log.Handler (setFormatter)
import System.Log.Formatter (simpleLogFormatter)
import Control.Exception (catch, SomeException)

--    Target = host command
data Target = Target {
    targetAddress :: String,
    targetCommand :: String
} deriving (Show)

getPassword :: String -> IO String
getPassword prompt = do
  putStr prompt
  hFlush stdout
  oldEchoState <- hGetEcho stdin
  hSetEcho stdin False
  pass <- getLine
  hSetEcho stdin oldEchoState
  putStrLn ""
  return pass

getLineWithPrompt :: String -> IO String
getLineWithPrompt prompt = do
    putStr prompt
    hFlush stdout
    getLine


readTargetLine :: String -> IO Target
readTargetLine line = return $ Target host (drop (length host) line)
    where
        host = head $ words line


loadTargetsFrom :: FilePath -> IO [Target]
loadTargetsFrom path = do
    content <- readFile path
    mapM readTargetLine (lines content)

openEnvironment :: IO()
openEnvironment = do
    initialize True
    h' <- fileHandler "clustercute.log" DEBUG
    let h = setFormatter h' (simpleLogFormatter "$time $loggername: $msg")
    updateGlobalLogger "out" (addHandler h . setLevel INFO)
    updateGlobalLogger "exit" (addHandler h . setLevel INFO)
    updateGlobalLogger "progress" (setLevel INFO)

closeEnvironment :: IO()
closeEnvironment = do
    exit
    stopGlobalPool

runOnTargets :: String -> String -> [Target] -> IO ()
runOnTargets username password targets = do
    openEnvironment
    parallel_ $ map (executeOnTarget username password) targets
    closeEnvironment

executeOnTarget :: String -> String -> Target -> IO ()
executeOnTarget username password target = do
    infoM "progress" ("start " ++ host)

    catch remoteOperation (\e -> infoM "exit" (host ++ " failed " ++ show (e :: SomeException)))
    infoM "progress" ("finish " ++ host)
    return ()
    where
        host = targetAddress target
        remoteOperation = do
            (status, outputs) <- withSSH2User "/dev/null" username password host 22 (\session -> execCommands session [targetCommand target])
            mapM_ (\output -> infoM "out" (host ++ ": " ++ show output)) outputs
            infoM "exit" (host ++ " with " ++ show status)


runTargets :: String -> IO ()
runTargets targetFilePath = do
    username <- getLineWithPrompt "Username: "
    password <- getPassword "Password (first time): "
    passwordVerify <- getPassword "Password (second time): "
    if password == passwordVerify
        then do
            targets <- loadTargetsFrom targetFilePath
            runOnTargets username password targets
        else die "Different passwords given, exiting"
