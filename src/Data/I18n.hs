{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Data.I18n (
    Translations,
    loadTranslations,
    translate,
    translateBeamState,
    translateAudit
) where

import Data.Aeson
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import Data.Types (BeamState(..))

type Translations = HM.HashMap T.Text (HM.HashMap T.Text T.Text)

loadTranslations :: FilePath -> IO Translations
loadTranslations path = do
    content <- BL.readFile path
    case decode content of
        Just t  -> return t
        Nothing -> do
            putStrLn "Failed to load translations, falling back to empty"
            return HM.empty

translate :: Translations -> T.Text -> T.Text -> T.Text -> T.Text
translate t lang key defaultVal =
    case HM.lookup lang t of
        Just langMap -> HM.lookupDefault defaultVal key langMap
        Nothing      -> 
            case HM.lookup "en" t of
                Just enMap -> HM.lookupDefault defaultVal key enMap
                Nothing    -> defaultVal

translateBeamState :: Translations -> T.Text -> BeamState -> T.Text
translateBeamState t lang state = 
    let key = case state of
            BeamOn   -> "BeamOn"
            BeamOff  -> "BeamOff"
            BeamHold -> "BeamHold"
    in translate t lang key (T.pack $ show state)

translateAudit :: Translations -> T.Text -> BeamState -> BeamState -> String
translateAudit t lang oldState newState =
    let msgKey = "AuditStateChanged"
        fmt = translate t lang msgKey "Beam State Changed: %s -> %s"
        oldStr = translateBeamState t lang oldState
        newStr = translateBeamState t lang newState
        -- simple replacement since Text.Printf is not working with Text nicely
        -- Wait, if there are two %s, replace one by one
        -- actually, let's just use a string replacement or format string.
    in T.unpack $ replaceTwo "%s" oldStr newStr fmt
  where
    replaceTwo needle r1 r2 haystack =
        let (prefix, rest) = T.breakOn needle haystack
        in if T.null rest 
           then haystack
           else let secondPart = T.drop (T.length needle) rest
                    (prefix2, rest2) = T.breakOn needle secondPart
                in if T.null rest2
                   then prefix <> r1 <> secondPart
                   else prefix <> r1 <> prefix2 <> r2 <> T.drop (T.length needle) rest2
