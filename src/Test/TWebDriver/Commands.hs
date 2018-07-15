{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}

module Test.TWebDriver.Commands where

import qualified Test.WebDriver.Monad as WDM
import           Test.WebDriver.Commands hiding (Element, Selector, click)
import qualified Test.WebDriver.Commands as WDM
import qualified Test.WebDriver.Commands.Internal as WDM
import           Data.Kind
import qualified Data.Aeson as JSON
import Test.TWebDriver.Commands.TH

-- * Web Elements

data ElementType = Select | A | Button | RadioButton | Input | B | I | Span

data Element (ty :: ElementType) = Element { unElement :: WDM.Element }
  deriving (Eq, Ord, Read, Show)

instance JSON.ToJSON (Element ty) where
  toJSON = JSON.toJSON . unElement

instance JSON.FromJSON (Element ty) where
  parseJSON = fmap Element . JSON.parseJSON

-- * Selectors

newtype Selector (ty :: ElementType) = Selector { unSelector :: WDM.Selector }
  deriving (Eq, Show, Ord)

instance JSON.ToJSON (Selector ty) where
  toJSON = JSON.toJSON . unSelector

type family Clickable (ty :: ElementType) :: Constraint where
  Clickable A = ()
  Clickable RadioButton = ()
  Clickable Select = ()

-- * Commands

click :: Clickable ty => Element ty -> WDM.WD ()
click = WDM.click . unElement

$(mkXPath "wow" "yis")

whoa :: String
whoa = unXPath wow
