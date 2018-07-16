{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib
    ( someFunc
    ) where

--------------------------------------------------------------------------------
import           Test.TWebDriver.Commands
--------------------------------------------------------------------------------

myS :: WD ()
myS = click =<< findElem $(mkXPath "//a/button/descendant::a[contains(text(), 'wow')]")

{-

click =<< findElement $ ByXPath "//button"

-}
someFunc :: IO ()
someFunc = myS `seq` pure ()
