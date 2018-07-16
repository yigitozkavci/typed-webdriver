{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.TWebDriver.TH where

--------------------------------------------------------------------------------
import           Data.Functor               (($>))
import           Language.Haskell.TH
--------------------------------------------------------------------------------
import           Test.TWebDriver.Capability
--------------------------------------------------------------------------------

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


