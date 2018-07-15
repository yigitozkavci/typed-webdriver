{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.TWebDriver.Commands where

import qualified Test.WebDriver.Monad as WDM
import           Test.WebDriver.Commands hiding (Element, Selector (..), click, ByXPath)
import qualified Test.WebDriver.Commands as WDM
import qualified Test.WebDriver.Commands.Internal as WDM
import           Data.Kind
import qualified Data.Aeson as JSON
import Test.TWebDriver.Commands.TH
import qualified Data.Text as T
import Data.Proxy
import GHC.TypeLits
import Data.Reflection
import Language.Haskell.TH
import Control.Monad

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

type family DecideElemTy (sym :: Symbol) :: ElementType where
  DecideElemTy "button" = Button

class KnownSymbol sym => Clickable (sym :: Symbol) where
instance Clickable "button"
instance Clickable "select"
instance Clickable "radio"
instance Clickable "a"

myFindElem :: Selector a -> WDM.WD (Element a)
myFindElem (Selector sel) = Element <$> findElem sel

isClickable :: String -> Bool
isClickable _ = True

prepareXPath' :: String -> DecsQ
prepareXPath' name = do
  if (isClickable name) then create_instance else pure []
  where
    create_instance =
      let className = mkName "Clickable"
      in pure [InstanceD Nothing [] (AppT (ConT className) (LitT (StrTyLit "//button/span"))) []]

instance JSON.ToJSON (Selector ty) where
  toJSON = JSON.toJSON . unSelector

-- * Commands

easyClick :: Clickable sym => Proxy sym -> WDM.WD ()
easyClick proxy = WDM.click =<< WDM.findElem (WDM.ByXPath (T.pack (symbolVal proxy)))
