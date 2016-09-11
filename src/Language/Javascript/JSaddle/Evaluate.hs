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
    eval
) where

import Language.Javascript.JSaddle.Types (JSVal)
#ifdef ghcjs_HOST_OS
import GHCJS.Types (nullRef)
import GHCJS.Marshal.Pure (pFromJSVal)
import Language.Javascript.JSaddle.Types (JSString)
#else
import Language.Javascript.JSaddle.Native
       (wrapJSVal, withJSString)
import Language.Javascript.JSaddle.WebSockets
       (Command(..), Result(..), sendCommand)
#endif
import Language.Javascript.JSaddle.Classes
       (ToJSString(..))
import Language.Javascript.JSaddle.Monad (JSM)

-- $setup
-- >>> import Language.Javascript.JSaddle.Test (testJSaddle)
-- >>> import Language.Javascript.JSaddle.Value (valToText)
-- >>> import Language.Javascript.JSaddle.Object (global, array, (!))
-- >>> import Language.Javascript.JSaddle.Monad (catch)
-- >>> import Language.Javascript.JSaddle.Exception (JSException(..))
-- >>> testJSaddle $ return ()
-- ...

-- | Evaluates a script (like eval in java script)
--
-- >>> testJSaddle $ eval "1+1"
-- 2
eval :: (ToJSString script)
     => script         -- ^ JavaScript to evaluate
     -> JSM JSVal
#ifdef ghcjs_HOST_OS
eval script = liftIO $ js_eval (toJSString script)
foreign import javascript unsafe "$r = eval($1);"
    js_eval :: JSString -> IO JSVal
#else
eval script = do
    rscript <- toJSString script
    withJSString rscript $ \script' -> do
        EvaluateScriptResult result <- sendCommand $ EvaluateScript script'
        wrapJSVal result
#endif
