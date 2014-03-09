{-# LANGUAGE CPP #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | This package provides an EDSL for calling JavaScript code using
--   the JavaScriptCore engine and low level Haskell bindings
--   in the webkit-javascriptcore library <https://github.com/ghcjs/webkit-javascriptcore>.
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle (
  -- * JSaddle EDSL
  -- | The 'JSM' monad gives us the context for evaluation.  In keeping
  --   with JavaScript the EDSL has
  --
  --   * /Weakish typing/ - type classes are used to convert to JSValueRef
  --   and JSObjectRef types
  --
  --   * /Strict evaluation/ - function in the 'JSM' monad can be passed in
  --   place of a value and will evaluated and converted to JSValueRef or
  --   JSObjectRef and then passed on to JavaScript
  --
  --   JSaddle should be used to write wrappers for JavaScript libraries that provide
  --   more type safety.

  -- * Code Examples
  -- | The code examples in this documentation are executed with a 'runjs'
  --   function that executes the example code in the JSM monad and converts
  --   the result to 'Text' with 'valToText'.  It also catches unhandled
  --   exceptions with 'catch'.  The source code can be found in tests/TestJSaddle.hs
  --
  --   Where it makes sense code examples are given in two forms.  One
  --   that uses 'eval' to run a purely JavaScript version and one that
  --   uses more of the JSaddle EDSL feature being demonstated.

  -- * Calling Haskell from JavaScript
  -- | You can call back into haskell from JavaScript using 'fun' to
  --   convert a Haskell function in the JSM monad into a javascript
  --   value.

  -- * JMacro Support
  -- | If you want a more JavaScript like syntax we also have a
  --   JMacro based QuasiQuoter 'evalJM'.  This converts your JMacro
  --   code into a string literal and a call to @eval@ to evaluate it
  --   using JavaScriptCore.

  -- * GHCJS Support
  -- | Because it uses webkit-javascriptcore you can compile your JSaddle code to
  --   JavaScript using GHCJS <https://github.com/ghcjs/ghcjs> and run it
  --   in a web browser.  Calls to the WebKitGTK+ JavaScriptCore C functions
  --   will be replaced with JavaScript function calls.

  -- * Modules
    module JSaddle
) where

import Language.Javascript.JSaddle.Monad as JSaddle
import Language.Javascript.JSaddle.Exception as JSaddle
import Language.Javascript.JSaddle.Value as JSaddle
import Language.Javascript.JSaddle.Arguments as JSaddle
import Language.Javascript.JSaddle.Properties as JSaddle
import Language.Javascript.JSaddle.Object as JSaddle
import Language.Javascript.JSaddle.Evaluate as JSaddle
#ifdef MIN_VERSION_jmacro
import Language.Javascript.JSaddle.JMacro as JSaddle
#endif
import Language.Javascript.JSaddle.String as JSaddle
