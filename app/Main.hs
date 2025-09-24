{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Process 
import System.Exit (ExitCode(..))
import Data.List (isInfixOf)

data GitAction = Ready | Pull | Push   deriving (Show)

main :: IO ()
main = do
  putStrLn "Ας ξεκινήσουμε τον έλεγχο του git"
  gitStatus <- checkFetch
  case gitStatus of
    Nothing -> putStrLn "problem!"
    Just x -> putStrLn $ show x 

checkFetch :: IO ( Maybe GitAction)
checkFetch = do
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode
    (proc "git" ["fetch", "origin"]) ""
  case exitCode of
    ExitSuccess -> do
      putStrLn ""
      let lns = lines stdout
      if null lns
        then do
          print lns
          putStrLn $ "Θέλει διερεύνηση to fetch  "  
          pullStatus <- checkPull
          case pullStatus of
            Nothing -> putStrLn "error on pull check"
            Just _ -> putStrLn "ok"
          return $ pullStatus
        else do
          print lns
          return $  Just Ready
    ExitFailure _ -> do
      -- Εδώ ελέγχουμε αν το stderr περιέχει το μήνυμα που θέλουμε να "καλύψουμε"
      putStrLn "βρήκαμε λάθος!"
      if "not a git repository" `isInfixOf` stderr
        then putStrLn "Δεν υπάρχει git repository!!"
        else putStrLn $ "Σφάλμα git: " ++ stderr
      return Nothing


checkPull :: IO ( Maybe GitAction)
checkPull = do
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode
    (proc "git" ["rev-list", "--count", "HEAD..origin/main"]) ""
  case exitCode of
    ExitSuccess -> do
      putStrLn "sucees pull"
      let lns = lines stdout
      if null lns
        then do
          print lns
          putStrLn $ "keno  revist  "
          return $ Just Ready
        else do
          print lns
          putStrLn $ "Θέλει διερεύνηση to revist  "++ show ((read (head lns))::Int)
          let needPull = checkReturnValueFromGit lns
          if needPull
            then return $ Just Pull
            else return $ Just Ready 
    ExitFailure _ -> do
      -- Εδώ ελέγχουμε αν το stderr περιέχει το μήνυμα που θέλουμε να "καλύψουμε"
      if "not a git repository" `isInfixOf` stderr
        then putStrLn "Δεν υπάρχει git repository!!"
        else putStrLn $ "Σφάλμα git: " ++ stderr
      return Nothing

checkReturnValueFromGit :: [String] -> Bool
checkReturnValueFromGit x = if ((read (head x))::Int) == 0
                            then False
                            else True
