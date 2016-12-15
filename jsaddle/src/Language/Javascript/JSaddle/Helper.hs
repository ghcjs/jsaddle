{-# LANGUAGE CPP #-}
module Language.Javascript.JSaddle.Helper where

import GHCJS.Types (JSVal)
import Language.Javascript.JSaddle.Types (JSM)

#ifdef ghcjs_HOST_OS
import GHCJS.Marshal.Pure (pFromJSVal, pToJSVal)
import JavaScript.TypedArray.ArrayBuffer (MutableArrayBuffer)
#else
import JavaScript.TypedArray.ArrayBuffer.Internal (MutableArrayBuffer, SomeArrayBuffer(..))
#endif

-- | Helper function needed because there is no FromJSVal instance for MutableArrayBuffer
mutableArrayBufferFromJSVal :: JSVal -> JSM MutableArrayBuffer
#ifdef ghcjs_HOST_OS
mutableArrayBufferFromJSVal = return . pFromJSVal
#else
mutableArrayBufferFromJSVal = return . SomeArrayBuffer
#endif
{-# INLINE mutableArrayBufferFromJSVal #-}

-- | Helper function needed because there is no ToJSVal instance for MutableArrayBuffer
mutableArrayBufferToJSVal :: MutableArrayBuffer -> JSM JSVal
#ifdef ghcjs_HOST_OS
mutableArrayBufferToJSVal = return . pToJSVal
#else
mutableArrayBufferToJSVal (SomeArrayBuffer v) = return v
#endif
{-# INLINE mutableArrayBufferToJSVal #-}
