JSaddle
=======

JSaddle is an interface for JavaScript that works with GHCJS or GHC.  It is used by [ghcjs-dom](https://github.com/ghcjs/ghcjs-dom)
when compiled with GHC and is compatible with [ghcjs-dom](https://github.com/ghcjs/ghcjs-dom) when compiled with GHCJS.

You can use JSaddle directly as follows:

``` Haskell
module Main ( main ) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)
import Control.Lens ((^.))
import Language.Javascript.JSaddle
       (run, jsg, js, js1, jss, fun, valToNumber, syncPoint)

main = run 3709 $ do
    doc <- jsg "document"
    doc ^. js "body" ^. jss "innerHTML" "<h1>Kia ora (Hi)</h1>"

    -- Create a haskell function call back for the onclick event
    doc ^. jss "onclick" (fun $ \ _ _ [e] -> do
        x <- e ^. js "clientX" >>= valToNumber
        y <- e ^. js "clientY" >>= valToNumber
        newParagraph <- doc ^. js1 "createElement" "p"
        newParagraph ^. js1 "appendChild" (
            doc ^. js1 "createTextNode" ("Click " ++ show (x, y)))
        doc ^. js "body" ^. js1 "appendChild" newParagraph
        return ())

    -- Make an exit button
    exitMVar <- liftIO newEmptyMVar
    exit <- doc ^. js1 "createElement" "span"
    exit ^. js1 "appendChild" (
        doc ^. js1 "createTextNode" "Click here to exit")
    doc ^. js "body" ^. js1 "appendChild" exit
    exit ^. jss "onclick" (fun $ \ _ _ _ -> liftIO $ putMVar exitMVar ())

    -- Force all all the lazy evaluation to be executed
    syncPoint

    -- In GHC compiled version the WebSocket connection will end when this
    -- thread ends.  So we will wait until the user clicks exit.
    liftIO $ takeMVar exitMVar
    doc ^. js "body" ^. jss "innerHTML" "<h1>Ka kite ano (See you later)</h1>"
    return ()
```

When compiled with GHC this code will run a Warp web server you can connect to at `http:\\localhost:3709\`.

Here is the same program using ghcjs-dom to call JSaddle.  As well as better type safety this version uses
JS FFI directly (rather than via JSaddle) when compiled with GHCJS:

``` Haskell
module Main (
    main
) where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Concurrent.MVar (takeMVar, putMVar, newEmptyMVar)

import GHCJS.DOM (run, syncPoint, currentDocument)
import GHCJS.DOM.Document (getBody, createElement, createTextNode)
import GHCJS.DOM.Element (setInnerHTML)
import GHCJS.DOM.Node (appendChild)
import GHCJS.DOM.EventM (on, mouseClientXY)
import qualified GHCJS.DOM.Document as D (click)
import qualified GHCJS.DOM.Element as E (click)

main = run 3708 $ do
    Just doc <- currentDocument
    Just body <- getBody doc
    setInnerHTML body (Just "<h1>Kia ora (Hi)</h1>")
    on doc D.click $ do
        (x, y) <- mouseClientXY
        Just newParagraph <- createElement doc (Just "p")
        text <- createTextNode doc $ "Click " ++ show (x, y)
        appendChild newParagraph text
        appendChild body (Just newParagraph)
        return ()
    
    -- Make an exit button
    exitMVar <- liftIO newEmptyMVar
    Just exit <- createElement doc (Just "span")
    text <- createTextNode doc "Click here to exit"
    appendChild exit text
    appendChild body (Just exit)
    on exit E.click $ liftIO $ putMVar exitMVar ()

    -- Force all all the lazy evaluation to be executed
    syncPoint

    -- In GHC compiled version the WebSocket connection will end when this
    -- thread ends.  So we will wait until the user clicks exit.
    liftIO $ takeMVar exitMVar
    setInnerHTML body (Just "<h1>Ka kite ano (See you later)</h1>")
    return ()
```

When compiled with GHC this code will run a Warp web server you can connect to at `http:\\localhost:3708\`.

## How does it work on GHC

As of version 0.5 it runs a small warp server that provides [index.html](https://github.com/ghcjs/jsaddle/blob/master/data/index.html) and
[jsaddle.js](https://github.com/ghcjs/jsaddle/blob/master/data/jsaddle.js).  The [jsaddle.js](https://github.com/ghcjs/jsaddle/blob/master/data/jsaddle.js) code connects to the server using WebSockets and processes JSON commands from the server and sends the result back over the same WebSocket.

### Why use WebSockets?

Older versions relied on WebKitGTK and JavaScriptCore, but in WekKit2 the interface JSaddle used is only available to WebKit Extensions
so most of the benefits of this interface are lost (extensions are not in the same process as Gtk and RPC would have been needed).
The general advice for people migrating from WebKit1 to WebKit2 seems to be to use JavaScript.  There is an interface in WebKit2
to send JavaScript code from the host application, but it does not seem to provide any advantages over the WebSockets approach.

### JSVal

When compiling with GHC a `JSVal` is represented by a integer key for a JavaScript `Map` in `jsaddle.js` code.
The first five keys `[0..4]` are
`null`, `undefined`, `true`, `false` and `window`.  This means the Haskell code can quickly create a JSVal with one of
these values.  Both the Haskell code and `jsaddle.js` can allocate new `JSVal` keys.  The Haskell code allocates negative
keys and `jsaddle.js` allocates positive keys to avoid clashing.

The haskell code will not know the value of the to go with the key, but it will include it in commands to the server
that are sort of "hey call this function and and put the result in the map with this key").  This allows for the
lazy execution of the JSaddle code.

A finalizer is added on the Haskell code and an asynchronous `FreeJSVal` command is automatically sent to `jsaddle.js`
when the JSVal is no longer reachable.  The `jsaddle.js` code then deletes the key from the Map.

### JSStrings

These are haskell `Text` type and so reside on the native side of the WebSocket.  If you want to make avoid transferring large string values over the WebSocket use `toJSVal` to conevt it to a `JSVal`. 

### Lazy Execution

To improve performance commands are sent to `jsaddle.js` in batches that contain any number of asynchronous commands followed by one synchronous one.  To get the best performance you should avoid synchronous commands, these are:

| Synchronous Command | What will trigger it                                         |
| ------------------- | ------------------------------------------------------------ |
| **ValueToString**   | converting a `JSVal` to a `JSString` (`Text` when using GHC) |
| **ValueToBool**     | converting a `JSVal` to a `Bool`                             |
| **ValueToNumber**   | converting a `JSVal` to a `Double`                           |
| **ValueToJSON**     | converting a `JSVal` to a `JSON`                             |
| **DeRefVal**        | converting a `JSVal` to a `Value`                            |
| **IsNull**          | testing to see if a `JSVal` is `null`                        |
| **IsUndefined**     | testing to see if a `JSVal` is `undefined`                   |
| **InstanceOf**      | testing to see if a `JSVal` is an `instanceOf` a given type  |
| **StrictEqual**     | testing too `JSVal` values for equality (`===`)              |
| **PropertyNames**   | getting the list of properties in an JS object               |

So basically if you don't look inside a `JSVal` to find out something the state of the JavaScript context you will not produce any
synchronous commands and your code will run fast.  As soon as you look at what is in a `JSVal` we have to wait for `jsaddle.js` to
send the results back and your code will block.

In some cases you may wish to make sure all the JSaddle commands are executed.  You can use the `syncPoint` and `syncAfter` functions to
force all the pending asynchronous commands to be executed.


## How does it work on GHCJS

It uses a handful of JS FFI calls to execute JavaScript functions indirectly.  This indirection will be small compared to the overhead of the WebSockets approach (used when JSaddle is compiled with GHC), but it will be significant compared to hand crafted JS FFI calls.

For the best performance you may want to write both JS FFI and JSaddle wrappers
For your JavaScript code.  This is the approach taken by recent versions of `ghcjs-dom`.

For instance here is `getElementById` from the `ghcjs-dom-jsffi` (used by `ghcjs-dom` when compiled with GHCJS)

``` Haskell
foreign import javascript unsafe "$1[\"getElementById\"]($2)"
        js_getElementById :: Document -> JSString -> IO (Nullable Element)

getElementById ::
               (MonadIO m, IsDocument self, ToJSString elementId) =>
                 self -> elementId -> m (Maybe Element)
getElementById self elementId
  = liftIO
      (nullableToMaybe <$>
         (js_getElementById (toDocument self) (toJSString elementId)))
```

Here is `getElementById` from `jsaddle-dom`  (used by `ghcjs-dom` when compiled with GHC)

``` Haskell
getElementById ::
               (MonadDOM m, IsDocument self, ToJSString elementId) =>
                 self -> elementId -> m (Maybe Element)
getElementById self elementId
  = liftDOM
      (((toDocument self) ^. jsf "getElementById" [toJSVal elementId])
         >>= fromJSVal)
```

## Exceptions

JSaddle does not support exceptions well.  When compiled with GHCJS an exception will result in termination of the thread
at the point the exception is thrown.

When using GHC the Haskell executions will probably continue for a while before the exception is received at all by the Haskell code
(because the lazy execution of the JS code).  The exception will be thrown when the the next synchronous command is executed.
When it reaches the synchronous command a haskell type JSException will be thrown (this may not be the thread that initiated the
command that caused the exception).  You can use `syncPoint` and `syncAfter` to force the exception to be thrown.
There are `JSM` versions of `catch` and `bracket` that also include a `syncPoint` call.
