{-# LANGUAGE TemplateHaskell #-}
module SignalProcessing.Matrix where

import Language.Haskell.TH
import qualified Data.Vector.Unboxed as V

-- | Generates a Matrix newtype that wraps a flat Vector.
-- E.g. generateMatrix "Matrix3x3" 3 3
generateMatrix :: String -> Int -> Int -> Q [Dec]
generateMatrix nameStr rows cols = do
    let name = mkName nameStr
    let conName = mkName nameStr
    -- newtype MatrixNxM = MatrixNxM (Vector Double)
    let vecType = AppT (ConT (mkName "V.Vector")) (ConT (mkName "Double"))
    let con = NormalC conName [(Bang NoSourceUnpackedness SourceStrict, vecType)]
    let newtypeDec = NewtypeD [] name [] Nothing con []
    return [newtypeDec]
