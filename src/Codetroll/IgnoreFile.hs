{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Codetroll.IgnoreFile where

import           Shelly

import           Data.Either

import qualified Data.Text as T
import qualified Data.Text.ICU.Regex as RE

import           Data.Attoparsec.Combinator
import qualified Data.Attoparsec.Text as AT
import           Control.Applicative

import           Data.Traversable hiding (sequence)

import           Text.Shakespeare.Text

import           Control.Exception
import           Control.Monad

import           Prelude hiding (FilePath)
default (T.Text)

data IgnorePattern = IgnorePattern { fileNameRegex :: RE.Regex, codeRegex :: RE.Regex } deriving (Show)

isntTab :: Char -> Bool
isntTab c = c /= '\t'

isEndOfLineChar :: Char -> Bool
isEndOfLineChar c = c == '\n'

shouldIgnoreLine' :: FilePath -> T.Text -> IgnorePattern -> Sh Bool
shouldIgnoreLine' filePath line ignorePattern =
  let codeRegex' = codeRegex ignorePattern
      fileNameRegex' = fileNameRegex ignorePattern
      matchesFiles = do
        _ <- RE.setText fileNameRegex' $ toTextIgnore filePath
        RE.find fileNameRegex' (-1)
      matchesLine  = do
        _ <- RE.setText codeRegex' $ line
        RE.find codeRegex' (-1)
  in liftIO $ ((&&) <$> matchesLine) <*> matchesFiles

shouldIgnoreLine :: [ IgnorePattern ] -> (FilePath, T.Text) -> Sh Bool
shouldIgnoreLine patterns (path, matched) =
  or <$> (sequence $ (shouldIgnoreLine' path matched) <$> patterns)

ignoreFileParser :: FilePath -> AT.Parser (Sh IgnorePattern)
ignoreFileParser filePath = do
  filePattern <- (AT.takeWhile1 isntTab) <* (AT.take 1)
  codePattern <- AT.takeText 
  let
    exitOnError :: Either RE.ParseError RE.Regex -> Sh RE.Regex
    exitOnError (Left err) = errorExit [st|Error parsing ignore file #{show $ filePath}: #{show $ err}|]
    exitOnError (Right re) = return re
    codeRegex = (liftIO $ RE.regex' [] codePattern) >>= exitOnError
    fileRegex = (liftIO $ RE.regex' [] filePattern) >>= exitOnError
    ignorePattern = do
      code <- codeRegex
      file <- fileRegex
      return $ IgnorePattern file code
    in return ignorePattern

readIgnoreFile :: FilePath -> Sh [ IgnorePattern ]
readIgnoreFile path = do
  raw <- readfile path
  rawLines <- return $ T.lines raw
  parsedLines <- return $ (AT.parseOnly $ ignoreFileParser path) <$> rawLines
  sequence $ (\t ->
    case t of
      Left err -> errorExit $ T.unwords [ "Could not parse codetroll ignore file: ", (T.pack err) ]
      Right ignore -> ignore
    ) <$> parsedLines

readIgnoreFiles :: FilePath -> FilePath -> Sh [ IgnorePattern ]
readIgnoreFiles repository codeTrollDir = do
  personalIgnoreFiles   <- safeLs codeTrollDir (echo "WARNING: ~/.codetroll/whitelists not found.")
  repoFile              <- filterM test_f [ repository </> ".codetroll-ignore" ]
  _                     <- if null repoFile then echo "WARNING: .codetroll-ignore file not found." else return ()
  join <$> (sequence $ readIgnoreFile <$> (personalIgnoreFiles ++ repoFile))

safeLs :: FilePath -> Sh a -> Sh [ FilePath ]
safeLs path onError = 
  let catchAll :: SomeException -> Sh [ FilePath ]
      catchAll _ = onError *> (return [])
  in catch_sh (ls path) catchAll