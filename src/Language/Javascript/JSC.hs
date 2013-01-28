-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | This package provides an EDSL for calling JavaScript code using
--   the JavaScriptCore engine and low level Haskell bindings
--   in the webkit-javascriptcore library <https://github.com/ghcjs/webkit-javascriptcore>.
-----------------------------------------------------------------------------

module Language.Javascript.JSC (
  -- * JSC EDSL
  -- | The 'JSC' monad gives us the context for evaluation.  In keeping
  --   with JavaScript the EDSL has
  --
  --   * /Weakish typing/ - type classes are used to convert to JSValueRef
  --   and JSObjectRef types
  --
  --   * /Strict evaluation/ - function in the 'JSC' monad can be passed in
  --   place of a value and will evaluated and converted to JSValueRef or
  --   JSObjectRef and then passed on to JavaScript
  --
  --   JSC should be used to write wrappers for JavaScript libraries that provide
  --   more type safety.

  -- * Code Examples
  -- | The code examples in this documentation are executed with a 'runjs'
  --   function that executes the example code in the JSC monad and converts
  --   the result to 'Text' with 'valToText'.  It also catches unhandled
  --   exceptions with 'catch'.  The source code can be found in tests/TestJSC.hs
  --
  --   Where it makes sense code examples are given in two forms.  One
  --   that uses 'eval' to run a purely JavaScript version and one that
  --   uses more of the JSC EDSL feature being demonstated.

  -- * Calling Haskell from JavaScript
  -- | You can call back into haskell from JavaScript using 'fun' to
  --   convert a Haskell function in the JSC monad into a javascript
  --   value.

  -- * JMacro Support
  -- | If you want a more JavaScript like syntax we also have a
  --   JMacro based QuasiQuoter 'evalJM'.  This converts your JMacro
  --   code into a string literal and a call to @eval@ to evaluate it
  --   using JavaScriptCore.

  -- * GHCJS Support
  -- | Because it uses webkit-javascriptcore you can compile your JSC code to
  --   JavaScript using GHCJS <https://github.com/ghcjs/ghcjs> and run it
  --   in a web browser.  Calls to the WebKitGTK+ JavaScriptCore C functions
  --   will be replaced with JavaScript function calls.

  -- * Modules
    module Language.Javascript.JSC.Monad
  , module Language.Javascript.JSC.Exception
  , module Language.Javascript.JSC.Value
  , module Language.Javascript.JSC.Arguments
  , module Language.Javascript.JSC.Properties
  , module Language.Javascript.JSC.Object
  , module Language.Javascript.JSC.Evaluate
  , module Language.Javascript.JSC.JMacro
  , module Language.Javascript.JSC.String
) where

import Language.Javascript.JSC.Monad
import Language.Javascript.JSC.Exception
import Language.Javascript.JSC.Value
import Language.Javascript.JSC.Arguments
import Language.Javascript.JSC.Properties
import Language.Javascript.JSC.Object
import Language.Javascript.JSC.Evaluate
import Language.Javascript.JSC.JMacro
import Language.Javascript.JSC.String
