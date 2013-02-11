-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.Evaluate
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | If you just want to run some JavaScript that you have as a string this is
--   you can use 'eval' or 'evaluateScript'.
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.Evaluate (
    evaluateScript
  , eval
) where

import Control.Monad.Trans.Reader (ask)
import Control.Monad.IO.Class (MonadIO(..))
import Foreign (nullPtr)
import Graphics.UI.Gtk.WebKit.JavaScriptCore.JSBase
       (JSStringRef, JSValueRef, JSObjectRef, jsevaluatescript,
        JSValueRefRef)
import Language.Javascript.JSC.Exception (rethrow)
import Language.Javascript.JSC.Value ()
import Language.Javascript.JSC.Object ()
import Language.Javascript.JSC.Classes
       (MakeObjectRef(..), MakeStringRef(..))
import Language.Javascript.JSC.Monad (JSC)


-- | Evaluates a script (like eval in java script).  Unlike 'eval' this function lets you
--   specify a source URL and starting line number for beter error information.
--
-- >>> testJSC $ (evaluateScript "\n\n{" global "FileName" 53 >>= valToText) `catch` \(JSException e) -> array (e,e!"sourceURL", e!"line") >>= valToText
-- SyntaxError: Expected token '}',FileName,55
evaluateScript :: (MakeStringRef script, MakeObjectRef this, MakeStringRef url)
               => script         -- ^ JavaScript to evaluate
               -> this
               -> url
               -> Int            -- ^ The Line number of the first line of the script
               -> JSC JSValueRef
evaluateScript script this url line = do
    gctxt <- ask
    thisr <- makeObjectRef this
    rethrow $ liftIO . jsevaluatescript gctxt (makeStringRef script) thisr (makeStringRef url) line

-- | Evaluates a script (like eval in java script)
--
-- >>> testJSC $ eval "1+1"
-- 2
eval :: MakeStringRef script
     => script         -- ^ JavaScript to evaluate
     -> JSC JSValueRef
eval script = evaluateScript script (nullPtr::JSObjectRef) (nullPtr::JSStringRef) 1


