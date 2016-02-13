{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
#ifdef ghcjs_HOST_OS
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
import Language.Javascript.JSaddle.Types (JSVal)
#ifdef ghcjs_HOST_OS
import GHCJS.Types (nullRef)
import GHCJS.Marshal.Pure (pFromJSVal)
import Language.Javascript.JSaddle.Types (JSString)
#else
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (jsevaluatescript)
import Language.Javascript.JSaddle.Native
       (makeNewJSVal, withObject, withJSString)
#endif
import Language.Javascript.JSaddle.Exception (rethrow)
import Language.Javascript.JSaddle.String (nullJSString)
import Language.Javascript.JSaddle.Object (nullObject)
import Language.Javascript.JSaddle.Classes
       (MakeObject(..), ToJSString(..))
import Language.Javascript.JSaddle.Monad (JSM)

-- $setup
-- >>> import Language.Javascript.JSaddle.Test (testJSaddle)
-- >>> import Language.Javascript.JSaddle.Value (valToText)
-- >>> import Language.Javascript.JSaddle.Object (global, array, (!))
-- >>> import Language.Javascript.JSaddle.Monad (catch)
-- >>> import Language.Javascript.JSaddle.Exception (JSException(..))

-- | Evaluates a script (like eval in java script).  Unlike 'eval' this function lets you
--   specify a source URL and starting line number for beter error information.
--
-- >>> testJSaddle $ (evaluateScript "\n\n{" global "FileName" 53 >>= valToText) `catch` \(JSException e) -> array (e,e!"sourceURL", e!"line") >>= valToText
-- SyntaxError: Expected token '}',FileName,55
evaluateScript :: (ToJSString script, MakeObject this, ToJSString url)
               => script         -- ^ JavaScript to evaluate
               -> this
               -> url
               -> Int            -- ^ The Line number of the first line of the script
               -> JSM JSVal
#ifdef ghcjs_HOST_OS
evaluateScript script this url line = liftIO $ js_eval (toJSString script)
{-# INLINE evaluateScript #-}
foreign import javascript unsafe "$r = eval($1);"
    js_eval :: JSString -> IO JSVal
#else
evaluateScript script this url line = do
    gctxt <- ask
    rthis <- makeObject this
    result <-
        withJSString (toJSString script) $ \script' ->
            withObject rthis $ \this' ->
                withJSString (toJSString url) $ \url' ->
                    rethrow $ liftIO . jsevaluatescript gctxt script' this' url' line
    makeNewJSVal result
{-# INLINE evaluateScript #-}
#endif

-- | Evaluates a script (like eval in java script)
--
-- >>> testJSaddle $ eval "1+1"
-- 2
eval :: ToJSString script
     => script         -- ^ JavaScript to evaluate
     -> JSM JSVal
eval script = evaluateScript script nullObject nullJSString 1
{-# INLINE eval #-}
