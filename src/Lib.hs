{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Lib
    ( someFunc
    , module Test.TWebDriver.Commands
    ) where

import Test.TWebDriver.Commands
import Test.TWebDriver

myS :: Selector Button
myS = Selector $ ByXPath "//button"

someFunc :: IO ()
someFunc = print wow
