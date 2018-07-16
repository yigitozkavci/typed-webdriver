{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module Lib
    ( someFunc
    ) where

import Test.TWebDriver.Commands
import Test.TWebDriver

myS :: WD ()
myS = click =<< myFindElem $(mkXPath "//a/button/descendant::a[contains(text(), 'wow')]")

{-

click =<< findElement $ ByXPath "//button"

-}
someFunc :: IO ()
someFunc = myS `seq` pure ()
