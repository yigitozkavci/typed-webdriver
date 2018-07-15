{-# LANGUAGE TemplateHaskell #-}

module Test.TWebDriver.Commands.TH where

import Language.Haskell.TH

mkXPath :: String -> String -> DecsQ
mkXPath name val = do
  varName <- newName name
  pure [ValD (VarP varName) (NormalB (LitE (StringL val))) []]
