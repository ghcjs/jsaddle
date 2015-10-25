{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
#endif
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.Evaluate
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | If you just want to run some JavaScript that you have as a string this is
--   you can use 'eval' or 'evaluateScript'.
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Evaluate (
    evaluateScript
  , eval
) where

import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Language.Javascript.JSaddle.Types
       (JSStringRef, JSValueRef, Object(..),
        JSValueRefRef)
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
import GHCJS.Types (nullRef)
import GHCJS.Marshal.Pure (pFromJSVal)
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (jsevaluatescript)
import Foreign (nullPtr)
#endif
import Language.Javascript.JSaddle.Exception (rethrow)
import Language.Javascript.JSaddle.Value ()
import Language.Javascript.JSaddle.Object ()
import Language.Javascript.JSaddle.Classes
       (MakeObject(..), MakeStringRef(..))
import Language.Javascript.JSaddle.Monad (JSM)

-- | Evaluates a script (like eval in java script).  Unlike 'eval' this function lets you
--   specify a source URL and starting line number for beter error information.
--
-- >>> testJSaddle $ (evaluateScript "\n\n{" global "FileName" 53 >>= valToText) `catch` \(JSException e) -> array (e,e!"sourceURL", e!"line") >>= valToText
-- SyntaxError: Expected token '}',FileName,55
evaluateScript :: (MakeStringRef script, MakeObject this, MakeStringRef url)
               => script         -- ^ JavaScript to evaluate
               -> this
               -> url
               -> Int            -- ^ The Line number of the first line of the script
               -> JSM JSValueRef
#if defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)
evaluateScript script this url line = liftIO $ js_eval (makeStringRef script)
{-# INLINE evaluateScript #-}
foreign import javascript unsafe "$r = eval($1);"
    js_eval :: JSStringRef -> IO JSValueRef
#elif defined(USE_WEBKIT)
evaluateScript script this url line = do
    gctxt <- ask
    (Object thisr) <- makeObject this
    rethrow $ liftIO . jsevaluatescript gctxt (makeStringRef script) thisr (makeStringRef url) line
{-# INLINE evaluateScript #-}
#else
evaluateScript = undefined
#endif

-- | Evaluates a script (like eval in java script)
--
-- >>> testJSaddle $ eval "1+1"
-- 2
eval :: MakeStringRef script
     => script         -- ^ JavaScript to evaluate
     -> JSM JSValueRef
#if (defined(ghcjs_HOST_OS) && defined(USE_JAVASCRIPTFFI)) || !defined(USE_WEBKIT)
eval script = evaluateScript script (Object nullRef) (pFromJSVal nullRef::JSStringRef) 1
{-# INLINE eval #-}
#else
eval script = evaluateScript script (Object nullPtr) (nullPtr::JSStringRef) 1
{-# INLINE eval #-}
#endif
