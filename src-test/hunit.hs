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
