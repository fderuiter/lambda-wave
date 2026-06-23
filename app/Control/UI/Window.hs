module Control.UI.Window (
    initWindow
) where

import Foreign.C.Types
import Foreign.Ptr (nullPtr, Ptr)

foreign import ccall "init_gtk_ui" c_init_gtk_ui :: CInt -> Ptr (Ptr CChar) -> IO ()

-- | Initialize GTK Window
initWindow :: IO ()
initWindow = do
    c_init_gtk_ui 0 nullPtr
