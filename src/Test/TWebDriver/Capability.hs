{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module Test.TWebDriver.Capability where

--------------------------------------------------------------------------------
import           Text.XML.HXT.XPath.XPathDataTypes as X
import           Text.XML.HXT.XPath.XPathEval
--------------------------------------------------------------------------------
import           Control.Arrow                     ((>>>))
import           Data.Kind
import qualified Data.List.NonEmpty                as NE
import           GHC.TypeLits
import           Test.TWebDriver.ElementType
--------------------------------------------------------------------------------

data Capability =
    Clickable
  | Hoverable
  | ContainsText
  deriving Show

getTargetElement :: [XStep] -> Maybe ElementType
getTargetElement [] = Nothing
getTargetElement (x:xs) =
  let Step _ (NameTest (show -> elemTy)) _ = NE.last (x NE.:| xs)
  in
    case elemTy of
      "\"button\"" -> Just Button
      "\"a\""      -> Just A
      "\"span\""   -> Just Span
      "\"select\"" -> Just Select
      "\"radio\""  -> Just RadioButton
      "\"input\""  -> Just Input
      "\"b\""      -> Just B
      "\"i\""      -> Just I
      _            -> Nothing

generateCapabilities :: String -> Either String [Capability]
generateCapabilities = parseXPathExpr >>> \case
  -- Right (PathExpr _ (Just (LocPath _ (_:Step Child (NameTest (qname)) []:_)))) ->
  Right (PathExpr _ (Just (LocPath _ steps))) ->
    case getTargetElement steps of
      Nothing -> Left "Could not find the target element in xpath"
      Just elemTy -> pure $
        case elemTy of
          Button      -> [Clickable, Hoverable]
          A           -> [Clickable]
          Span        -> [ContainsText]
          Select      -> [Clickable]
          RadioButton -> [Clickable]
          Input       -> [Clickable]
          B           -> []
          I           -> []
  Right _ -> Left "XPath parsable but something is wrong with its structure"
  Left _ -> Left "XPath not parsable"

type family CanClick (name :: Symbol) (xs :: [Capability]) :: Constraint where
  CanClick name '[] = CatError name
  CanClick _ ('Clickable ': xs) = ()
  CanClick name (x ': xs) = CanClick name xs

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

