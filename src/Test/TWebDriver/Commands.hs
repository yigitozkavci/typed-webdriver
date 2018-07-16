{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
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
import           Test.WebDriver.Commands hiding (Element (..), Selector (..), click)
import qualified Test.WebDriver.Commands as WDM
import           Data.Kind hiding (Type)
import qualified Data.Aeson as JSON
import qualified Data.Text as T
import Data.Proxy
import GHC.TypeLits
import Language.Haskell.TH
import Text.XML.HXT.XPath.XPathEval
import Text.XML.HXT.XPath.XPathDataTypes as X
import Control.Arrow ((>>>))
import qualified Data.List.NonEmpty as NE
import Data.Functor (($>))

-- * Web Elements

data ElementType = Select | A | Button | RadioButton | Input | B | I | Span

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

myFindElem :: forall name cap. KnownSymbol name => Selector name cap -> WDM.WD (Element name cap)
myFindElem Selector = Element <$> findElem (WDM.ByXPath $ T.pack $ symbolVal (Proxy :: Proxy name))

-- * Commands

data Capability =
    Clickable
  | Hoverable
  | ContainsText
  deriving Show

toPromotedTH :: Either String [Capability] -> Q Type
toPromotedTH (Left err) = reportError err $> PromotedNilT
toPromotedTH (Right []) = pure PromotedNilT
toPromotedTH (Right (cap:xs)) = do
  deeper <- toPromotedTH (Right xs)
  pure $
    AppT
      (AppT
        PromotedConsT
        (PromotedT (mkName (show cap)))
      )
      deeper

getTargetElement :: [XStep] -> Maybe ElementType
getTargetElement [] = Nothing
getTargetElement (x:xs) =
  let Step _ (NameTest (show -> elemTy)) _ = NE.last (x NE.:| xs)
  in
    case elemTy of
      "\"button\"" -> Just Button
      "\"a\"" -> Just A
      "\"span\"" -> Just Span
      "\"select\"" -> Just Select
      "\"radio\"" -> Just RadioButton
      "\"input\"" -> Just Input
      "\"b\"" -> Just B
      "\"i\"" -> Just I
      _ -> Nothing

generateCapabilities :: String -> Either String [Capability]
generateCapabilities = parseXPathExpr >>> \case
  -- Right (PathExpr _ (Just (LocPath _ (_:Step Child (NameTest (qname)) []:_)))) ->
  Right (PathExpr _ (Just (LocPath _ steps))) ->
    case getTargetElement steps of
      Nothing -> Left "Could not find the target element in xpath"
      Just elemTy -> pure $
        case elemTy of
          Button -> [Clickable, Hoverable]
          A -> [Clickable]
          Span -> [ContainsText]
          Select -> [Clickable]
          RadioButton -> [Clickable]
          Input -> [Clickable]
          B -> []
          I -> []
  Right _ -> Left "XPath parsable but something is wrong with its structure"
  Left _ -> Left "XPath not parsable"

type family CatError (name :: Symbol) :: b where
  CatError name =
    TypeError ( 'Text "(.   \\"
          ':$$: 'Text "\\  |   "
          ':$$: 'Text "     \\ |___(\\--/)"
          ':$$: 'Text "   __/    (  . . )"
          ':$$: 'Text "  \"'._.    '-.O.'"
          ':$$: 'Text "       '-.  \\ \"|\\"
          ':$$: 'Text "          '.,,/'.,,mrf"
          ':$$: 'Text "The following XPath is not clickable:"
          ':$$: 'Text name
    )

mkXPath :: String -> Q Exp
mkXPath name = do
  promotedTySig <- toPromotedTH (generateCapabilities name)
  pure $
    SigE
      (ConE (mkName "Selector"))
      (AppT
        (AppT 
          (ConT (mkName "Selector"))
          (LitT (StrTyLit name))
        )
        promotedTySig
      )

type family CanClick (name :: Symbol) (xs :: [Capability]) :: Constraint where
  CanClick name '[] = CatError name
  CanClick _ ('Clickable ': xs) = ()
  CanClick name (x ': xs) = CanClick name xs

click :: forall name xs. CanClick name xs => Element name xs -> WDM.WD ()
click = WDM.click . unElement
