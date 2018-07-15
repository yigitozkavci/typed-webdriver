{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Lib
    ( someFunc
    , module Test.TWebDriver.Commands
    ) where

import Test.TWebDriver.Commands
import Test.TWebDriver
import Data.Proxy

prepareXPath' "//button/span"

myS :: WD ()
myS = easyClick $(withClassInstance "//button/span")

{-

click =<< findElement $ ByXPath "//button"

-}
someFunc :: IO ()
someFunc = pure ()
