{-# LANGUAGE TemplateHaskell #-}

module Control.WebUI.Assets (indexHtml) where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)

-- | The embedded HTML/JS/CSS content for the Web Dashboard.
-- This is compiled into the binary, removing the need for external asset files.
indexHtml :: ByteString
indexHtml = $(embedFile "app/Control/WebUI/assets/index.html")
