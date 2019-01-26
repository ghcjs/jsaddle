-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | This package provides an EDSL for calling JavaScript that
--   can be used both from GHCJS and GHC.  When using GHC
--   the application is run using Warp and WebSockets to
--   drive a small JavaScript helper.
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle (
  -- * JSaddle EDSL
  -- | The 'JSM' monad gives us the context for evaluation.  In keeping
  --   with JavaScript the EDSL has
  --
  --   * /Weakish typing/ - type classes are used to convert to JSValueRef
  --   and Object types
  --
  --   * /Strict evaluation/ - function in the 'JSM' monad can be passed in
  --   place of a value and will evaluated and converted to JSValueRef or
  --   Object and then passed on to JavaScript
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
  --   uses more of the JSaddle EDSL feature being demonstrated.

  -- * Calling Haskell from JavaScript
  -- | You can call back into Haskell from JavaScript using 'fun' to
  --   convert a Haskell function in the JSM monad into a javascript
  --   value.

  -- * GHCJS Support
  -- | When built with ghcjs the code works using JavaScript FFI by default.

  -- * GHC Support
  -- | When built with ghc the code runs a small Warp server that provides
  --   index.html and jsaddle.js files.  When a browser is connected the
  --   code in jsaddle.js will open a WebSockets connection to the server
  --   and the server will run the Haskell code.  The JSaddle parts will
  --   be executed by sending commands back to the browser.

  --   Although the JavaScript code is executed in the strict order
  --   set out by the EDSL it is done asynchronously to the Haskell code.
  --   This improves the performance by reducing the number of round trips
  --   needed between the Haskell and JavaScript code.

  --

  -- * Modules
    module JSaddle
) where

import GHCJS.Marshal as JSaddle
import Language.Javascript.JSaddle.Types as JSaddle
import Language.Javascript.JSaddle.Classes as JSaddle
import Language.Javascript.JSaddle.Marshal.String as JSaddle ()
import Language.Javascript.JSaddle.Monad as JSaddle
import Language.Javascript.JSaddle.Exception as JSaddle
import Language.Javascript.JSaddle.Value as JSaddle
import Language.Javascript.JSaddle.Arguments as JSaddle ()
import Language.Javascript.JSaddle.Properties as JSaddle
import Language.Javascript.JSaddle.Object as JSaddle
import Language.Javascript.JSaddle.Evaluate as JSaddle
import Language.Javascript.JSaddle.String as JSaddle
