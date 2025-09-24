{-# LANGUAGE ScopedTypeVariables #-}
module Main where

import System.Process 
import System.Exit (ExitCode(..))
import Data.List (isInfixOf)

data GitAction = NoGit | Pull 

main :: IO ()
main = do
  (exitCode, stdout, stderr) <- readCreateProcessWithExitCode (proc "git" ["status"]) ""
  case exitCode of
    ExitSuccess -> do
      let lns = lines stdout
      if null lns
        then putStrLn "Δεν υπάρχει git!"
        else print lns
    ExitFailure _ -> do
      -- Εδώ ελέγχουμε αν το stderr περιέχει το μήνυμα που θέλουμε να "καλύψουμε"
      if "not a git repository" `isInfixOf` stderr
        then putStrLn "Δεν υπάρχει git repository!!"
        else putStrLn $ "Σφάλμα git: " ++ stderr

checkGit :: String -> Maybe GitAction
checkGit []  = Nothing
checkGit _ = Just Pull 



