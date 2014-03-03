--   Copyright 2014 Commonwealth Bank of Australia
--
--   Licensed under the Apache License, Version 2.0 (the "License");
--   you may not use this file except in compliance with the License.
--   You may obtain a copy of the License at
--
--     http://www.apache.org/licenses/LICENSE-2.0
--
--   Unless required by applicable law or agreed to in writing, software
--   distributed under the License is distributed on an "AS IS" BASIS,
--   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
--   See the License for the specific language governing permissions and
--   limitations under the License.

{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Codetroll.Main where

import           Shelly

import           Data.Either
import           Data.Maybe

import           Control.Applicative
import           Control.Exception
import           Control.Monad

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

import qualified Data.List as L
import           Data.Traversable hiding (sequence)

import           Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Text as AT

import           Text.Shakespeare.Text

import           Codetroll.Git
import           Codetroll.IgnoreFile

import           Prelude hiding (FilePath)
import           System.Environment
default (T.Text)

data PatternMatch = PatternMatch { path :: FilePath, line :: T.Text, lineNumber :: Int } deriving (Show)

toPathAndLine m = (Codetroll.Main.path m, Codetroll.Main.line m)

grep_ :: [ T.Text ] -> [ FilePath ] -> Sh [ PatternMatch ]
grep_ _ [] = return []
grep_ [] _ = return []
grep_ (p : ps) files = do
  raw         <- errExit False $ run "grep" $ [ "--color=always", "-nr", T.append (L.foldl (\acc el -> T.concat [ acc, "\\|", el ]) (T.append "'" p) ps) "'" ] ++ (toTextIgnore <$> files)
  grepStatus  <- lastExitCode
  _           <- if grepStatus /= 1 && grepStatus /= 0 then errorExit [st|Grep returned with status code #{show $ grepStatus}|]  else return ()
  parsedLines <- return $ (AT.parseOnly grepParser) <$> T.lines raw
  sequence $ (\t ->
    case t of
      Left err -> errorExit $ T.concat [ [st|Could not parse grep response: #{show err}\n|], raw ]
      Right result -> return result
    ) <$> parsedLines

grepParser :: AT.Parser PatternMatch
grepParser =
  let 
     isSeparator c = c /= ':'
  in do
    filePath    <- (AT.takeWhile1 isSeparator) <* (AT.take 1)
    lineNumber  <- (AT.takeWhile1 isSeparator) <* (AT.take 1)
    matchedText <- AT.takeText
    return $ PatternMatch (fromText filePath) matchedText (read $ T.unpack lineNumber)

--

printResults :: [ PatternMatch ] -> Sh ()
printResults results =
  let
    printOffendingMatch :: PatternMatch -> Sh () 
    printOffendingMatch m = echo [st|#{toTextIgnore $ Codetroll.Main.path m}: #{line m}|]
  in
    if null results then echo "No offending files were detected." *> exit 0 
    else  do
      _ <- echo "Found offending files - please remove violating text or add exceptions to <repo>/.codetroll-ignore"
      _ <- sequence $ printOffendingMatch <$> results
      quietExit (- 1)

--

realMain :: IO ()
realMain = shellyNoDir $ silently $ do
  args <- liftIO $ getArgs
  targs <- return $ T.pack <$> args

  repoDir <- absPath . fromText $ L.last targs
  commands <- return $ L.init targs

  home <- fromText <$> get_env_text "HOME"
  codetrollDir <- return $ home </> ".codetroll"

  ignorePatterns <- readIgnoreFiles repoDir (codetrollDir </> "whitelists")

  patterns <- 
    let catchAll :: SomeException -> Sh [ T.Text ]
        catchAll _ = (echo "WARNING: ~/.codetroll/patterns not found. Codetroll will not scan any files.") *> (exit 0) *> (return [])
    in catch_sh (T.lines <$> (readfile $ codetrollDir </> "patterns")) catchAll

  case commands of

    "all" : _ -> do
      files <- (gitTracked_ repoDir)
      hasChanges <- gitModifiedWorkingDir_ repoDir
      if hasChanges 
        then errorExit "The git working directory has been changed - we cannot scan the relevant files. Please stash your changes and try again."
        else do
          grepped <- grep_ patterns files
          withoutSafeMatches <- filterM (((shouldIgnoreLine ignorePatterns) . toPathAndLine) >=> (return . not)) grepped
          printResults withoutSafeMatches

    "modified" : _ -> do
      files        <- gitStatus_  repoDir
      filesToCheck <- return $ catMaybes $ map (\(s, f) -> if canCheckForModifications s then Just f else Nothing) files
      grepped      <- grep_ patterns filesToCheck
      withoutSafeMatches <- filterM (((shouldIgnoreLine ignorePatterns) . toPathAndLine) >=> (return . not)) grepped
      printResults withoutSafeMatches
    otherwise -> echo usage

usage :: T.Text
usage = "\
    \usage: codetroll [ mode ] [ directory ]  \n\
    \ \n\
    \  mode is either 'all' or 'modified'.\n\
    \    all  will scan all files in the git repository. \n\
    \    modified will scan only those files that have been changed. \n\
    \  directory is the absolute path to the git repository.\n"
