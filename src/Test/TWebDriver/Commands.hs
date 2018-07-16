{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Test.TWebDriver.Commands
  ( module Test.TWebDriver.Capability
  , module Test.WebDriver.Commands
  , module Test.WebDriver
  , mkXPath
  , Selector (..)
  , click
  , findElem
  ) where

--------------------------------------------------------------------------------
import           Test.WebDriver             hiding (Element (..), Selector (..),
                                             click, findElem)
import           Test.WebDriver.Commands    hiding (Element (..), Selector (..),
                                             click, findElem)
import qualified Test.WebDriver.Commands    as WDM
import qualified Test.WebDriver.Monad       as WDM
--------------------------------------------------------------------------------
import qualified Data.Aeson                 as JSON
import           Data.Proxy
import qualified Data.Text                  as T
import           GHC.TypeLits
--------------------------------------------------------------------------------
import           Test.TWebDriver.Capability
import           Test.TWebDriver.TH
--------------------------------------------------------------------------------

-- * Web Elements

data Element (name :: Symbol) (xs :: [Capability]) = Element { unElement :: WDM.Element }
  deriving (Eq, Ord, Read, Show)

instance JSON.ToJSON (Element name cap) where
  toJSON = JSON.toJSON . unElement

instance JSON.FromJSON (Element name cap) where
  parseJSON = fmap Element . JSON.parseJSON

-- * Selectors

data Selector (name :: Symbol) (xs :: [Capability]) = Selector

instance KnownSymbol name => JSON.ToJSON (Selector name cap) where
  toJSON _ = JSON.toJSON (WDM.ByXPath $ T.pack $ symbolVal (Proxy :: Proxy name))

findElem :: forall name cap. KnownSymbol name => Selector name cap -> WDM.WD (Element name cap)
findElem Selector = Element <$> WDM.findElem (WDM.ByXPath $ T.pack $ symbolVal (Proxy :: Proxy name))

click :: forall name xs. CanClick name xs => Element name xs -> WDM.WD ()
click = WDM.click . unElement
