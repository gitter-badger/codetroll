{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Test.Framework
import Test.Framework.TH
import Test.Framework.Providers.HUnit

import Test.HUnit hiding (test)

import Codetroll.Git
import Codetroll.IgnoreFile

import qualified Data.Attoparsec.Text as AT

import qualified Data.Text as T
default (T.Text)

case_ParseGitStatusEntry = do 
  (AT.parseOnly parseGitStatusEntry "?? Test.hs") @?= (Right (Untracked, "Test.hs"))
  (AT.parseOnly parseGitStatusEntry "M  Test.hs") @?= (Right (Modified, "Test.hs"))
  (AT.parseOnly parseGitStatusEntry "A  Test.hs") @?= (Right (Added, "Test.hs"))
  (AT.parseOnly parseGitStatusEntry "D  Test.hs") @?= (Right (Deleted, "Test.hs"))
  (AT.parseOnly parseGitStatusEntry "R  Test.hs") @?= (Right (Renamed, "Test.hs"))
  (AT.parseOnly parseGitStatusEntry "C  Test.hs") @?= (Right (Added, "Test.hs"))
  (AT.parseOnly parseGitStatusEntry "U  Test.hs") @?= (Right (Modified, "Test.hs"))

main :: IO ()
main = $(defaultMainGenerator)
