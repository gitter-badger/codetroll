{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Codetroll.Git where


import           Shelly

import           Data.Either

import qualified Data.Text as T

import qualified Data.List as L

import           Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Text as AT
import           Control.Applicative

import           Prelude hiding (FilePath)
default (T.Text)


-- ?? untracked
-- M modified
-- A added
-- D deleted
-- R renamed
-- C copied
-- U updated but unmerged
data GitStatus = Untracked | Modified | Added | Deleted | Renamed | WorkTreeChangedButNotIndex deriving (Show, Eq)

parseGitStatus :: AT.Parser GitStatus
parseGitStatus = do
     (AT.string "??" >> return Untracked)
 <|> (AT.string " " <* (AT.take 1) >> return WorkTreeChangedButNotIndex)
 <|> (AT.string "M" <* (AT.take 1) >> return Modified)
 <|> (AT.string "A" <* (AT.take 1) >> return Added)
 <|> (AT.string "D" <* (AT.take 1) >> return Deleted)
 <|> (AT.string "R" <* (AT.take 1) >> return Renamed)
 <|> (AT.string "C" <* (AT.take 1) >> return Added)
 <|> (AT.string "U" <* (AT.take 1) >> return Modified)

parseGitStatusEntry :: AT.Parser (GitStatus, T.Text)
parseGitStatusEntry = do
  status   <- parseGitStatus
  _        <- AT.char ' '
  filename <- AT.takeText
  return $ (status, filename)

-- Find all the files that have been changed/modified in the git directory.
gitStatus_ :: FilePath -> Sh [ (GitStatus, FilePath) ]
gitStatus_ directory =
  let getFileStatuses = do
        cd directory
        raw <- run "git" ["status", "--porcelain", "-uall"]
        return $ T.lines raw
      parse statuses = AT.parseOnly parseGitStatusEntry (statuses)
  in do
    rawStatuses    <- getFileStatuses
    parsedStatuses <- return $ map parse rawStatuses
    textStatuses   <- sequence $ map (\t ->
      case t of
            Left err -> errorExit $ T.unwords [ "Could not parse git status result:", (T.pack err) ]
            Right (status, file) -> return $ (status, directory </> file)
            ) parsedStatuses
    return textStatuses

-- Returns true if the file exists and can be checked for changes.
-- (that is the file exists).
canCheckForModifications :: GitStatus -> Bool
canCheckForModifications  Modified = True
canCheckForModifications  Deleted  = False
canCheckForModifications  Renamed  = True
canCheckForModifications  WorkTreeChangedButNotIndex = False
canCheckForModifications  _        = True

-- Returns true if the working directory contains changes (that remove or modify files).
gitModifiedWorkingDir_ :: FilePath -> Sh Bool
gitModifiedWorkingDir_ path = do
  changes <- gitStatus_ path
  onlyModifies <- return $ let isModified Modified = True
                               isModified Deleted = True
                               isModified Renamed = True
                               isModified WorkTreeChangedButNotIndex = True
                               isModified _ = False
                  in filter (\(status, _) -> not $ isModified status) changes
  return $ not $ null changes

-- Returns all the tracked files in a git repository.
gitTracked_ :: FilePath -> Sh [ FilePath ]
gitTracked_ path = do
  cd path
  raw <- run "git" [ "ls-files" ]
  return $ (\rel -> path </> rel) <$> (T.lines raw)
