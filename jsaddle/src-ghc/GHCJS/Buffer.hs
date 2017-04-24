module GHCJS.Buffer
    ( Buffer
    , MutableBuffer
    , create
    , createFromArrayBuffer
    , thaw, freeze, clone
      -- * JavaScript properties
    , byteLength
    , getArrayBuffer
    , getUint8Array
    , getUint16Array
    , getInt32Array
    , getDataView
    , getFloat32Array
    , getFloat64Array
      -- * bytestring
    , toByteString, fromByteString
    ) where

import GHCJS.Buffer.Types

import Control.Lens.Operators ((^.))

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64 (encode, decode)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)

import qualified JavaScript.TypedArray.Internal.Types as I
import           JavaScript.TypedArray.ArrayBuffer.Internal (SomeArrayBuffer(..))
import           JavaScript.TypedArray.DataView.Internal    (SomeDataView(..))

import GHCJS.Marshal (FromJSVal(..))
import Language.Javascript.JSaddle.Types (JSM, GHCJSPure(..), ghcjsPure)
import Language.Javascript.JSaddle.Object (js, js2, jsg1, jsg3)

create :: Int -> JSM MutableBuffer
create n | n >= 0    = SomeBuffer <$> jsg1 "h$newByteArray" n
         | otherwise = error "create: negative size"
{-# INLINE create #-}

createFromArrayBuffer :: SomeArrayBuffer any -> GHCJSPure (SomeBuffer any)
createFromArrayBuffer (SomeArrayBuffer buf) = GHCJSPure $ SomeBuffer <$> jsg1 "h$wrapBuffer" buf
{-# INLINE createFromArrayBuffer #-}

getArrayBuffer :: SomeBuffer any -> GHCJSPure (SomeArrayBuffer any)
getArrayBuffer (SomeBuffer buf) = GHCJSPure $ SomeArrayBuffer <$> buf ^. js "buf"
{-# INLINE getArrayBuffer #-}

getInt32Array :: SomeBuffer any -> GHCJSPure (I.SomeInt32Array any)
getInt32Array (SomeBuffer buf) = GHCJSPure $ I.SomeTypedArray <$> buf ^. js "i3"
{-# INLINE getInt32Array #-}

getUint8Array :: SomeBuffer any -> GHCJSPure (I.SomeUint8Array any)
getUint8Array (SomeBuffer buf) = GHCJSPure $ I.SomeTypedArray <$> buf ^. js "u8"
{-# INLINE getUint8Array #-}

getUint16Array :: SomeBuffer any -> GHCJSPure (I.SomeUint16Array any)
getUint16Array (SomeBuffer buf) = GHCJSPure $ I.SomeTypedArray <$> buf ^. js "u1"
{-# INLINE getUint16Array #-}

getFloat32Array :: SomeBuffer any -> GHCJSPure (I.SomeFloat32Array any)
getFloat32Array (SomeBuffer buf) = GHCJSPure $ I.SomeTypedArray <$> buf ^. js "f3"
{-# INLINE getFloat32Array #-}

getFloat64Array :: SomeBuffer any -> GHCJSPure (I.SomeFloat64Array any)
getFloat64Array (SomeBuffer buf) = GHCJSPure $ I.SomeTypedArray <$> buf ^. js "f6"
{-# INLINE getFloat64Array #-}

getDataView :: SomeBuffer any -> GHCJSPure (SomeDataView any)
getDataView (SomeBuffer buf) = GHCJSPure $ SomeDataView  <$> buf ^. js "dv"
{-# INLINE getDataView #-}

freeze :: MutableBuffer -> JSM Buffer
freeze = js_clone
{-# INLINE freeze #-}

thaw :: Buffer -> JSM MutableBuffer
thaw  = js_clone
{-# INLINE thaw #-}

clone :: MutableBuffer -> JSM (SomeBuffer any2)
clone = js_clone
{-# INLINE clone #-}

fromByteString :: ByteString -> GHCJSPure (Buffer, Int, Int)
fromByteString bs = GHCJSPure $ do
  buffer <- SomeBuffer <$> jsg1 "h$newByteArrayFromBase64String" (decodeUtf8 $ B64.encode bs)
  return (buffer, 0, BS.length bs)
{-# INLINE fromByteString #-}

-- | Wrap a 'Buffer' into a 'ByteString' using the given offset
-- and length.
toByteString :: Int -> Maybe Int -> Buffer -> GHCJSPure ByteString
toByteString off mbLen buf = GHCJSPure $ do
  bufLen <- ghcjsPure $ byteLength buf
  case mbLen of
    _        | off < 0            -> error "toByteString: negative offset"
             | off > bufLen       -> error "toByteString: offset past end of buffer"
    Just len | len < 0            -> error "toByteString: negative length"
             | len > bufLen - off -> error "toByteString: length past end of buffer"
             | otherwise          -> ghcjsPure $ unsafeToByteString off len buf
    Nothing                       -> ghcjsPure $ unsafeToByteString off (bufLen - off) buf

unsafeToByteString :: Int -> Int -> Buffer -> GHCJSPure ByteString
unsafeToByteString off len (SomeBuffer buf) = GHCJSPure $ do
  b64 <- jsg3 "h$byteArrayToBase64String" off len buf >>= fromJSValUnchecked
  return $ case B64.decode (encodeUtf8 b64) of
            Left err -> error $ "unsafeToByteString base 64 decode error :" ++ err
            Right bs -> bs

byteLength :: SomeBuffer any -> GHCJSPure Int
byteLength (SomeBuffer buf) = GHCJSPure $ buf ^. js "len" >>= fromJSValUnchecked
{-# INLINE byteLength #-}

js_clone :: SomeBuffer any1 -> JSM (SomeBuffer any2)
js_clone (SomeBuffer buf) = SomeBuffer <$> jsg1 "h$wrapBuffer" (buf ^. js "buf" ^. js2 "slice" (buf ^. js "u8" ^. js "byteOffset") (buf ^. js "len"))
