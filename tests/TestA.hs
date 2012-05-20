{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
module TestA where
import Language.C.Simple.CType.Build.TH
import Language.C.Simple.CType.Build

data TestA = TestA {
        f0 :: Double,
        f1 :: Int
    }
    
