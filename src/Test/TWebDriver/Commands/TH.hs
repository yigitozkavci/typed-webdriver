{-# LANGUAGE TemplateHaskell #-}

module Test.TWebDriver.Commands.TH where

import Language.Haskell.TH

newtype XPath = XPath { unXPath :: String }

mkXPath :: String -> String -> DecsQ
mkXPath name val = do
  varName <- newName name
  xpName <- newName XPath
  pure [ValD (VarP varName) (NormalB (AppE (ConE xpName) (LitE (StringL val)))) []]
