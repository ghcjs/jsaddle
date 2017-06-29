{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.WebSockets.Files
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@gmail.com>
--
-- |
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.Run.Files (
    indexHtml
  , jsaddleJs
  , initState
  , runBatch
  , ghcjsHelpers
) where

import Data.ByteString.Lazy (ByteString)
import Data.Monoid ((<>))

indexHtml :: ByteString
indexHtml =
    "<!DOCTYPE html>\n\
    \<html>\n\
    \<head>\n\
    \<title>JSaddle</title>\n\
    \</head>\n\
    \<body>\n\
    \</body>\n\
    \<script src=\"jsaddle.js\"></script>\n\
    \</html>"

initState :: ByteString
initState = "\
    \        var jsaddle_values = new Map();\n\
    \        var jsaddle_free = new Map();\n\
    \        jsaddle_values.set(0, null);\n\
    \        jsaddle_values.set(1, undefined);\n\
    \        jsaddle_values.set(2, false);\n\
    \        jsaddle_values.set(3, true);\n\
    \        jsaddle_values.set(4, window);\n\
    \        var jsaddle_index = 100;\n\
    \        var expectedBatch = 1;\n\
    \        var lastResults = [0, {}];\n\
    \        var inCallback = 0;\n\
    \        var asyncBatch = null;\n\
    \"

runBatch :: (ByteString -> ByteString) -> Maybe (ByteString -> ByteString) -> ByteString
runBatch send sendSync = "\
    \  var runBatch = function(firstBatch, initialSyncDepth) {\n\
    \    var processBatch = function(timestamp) {\n\
    \      var batch = firstBatch;\n\
    \      var callbacksToFree = [];\n\
    \      var results = [];\n\
    \      inCallback++;\n\
    \      try {\n\
    \        syncDepth = initialSyncDepth || 0;\n\
    \        for(;;){\n\
    \          if(batch[2] === expectedBatch) {\n\
    \            expectedBatch++;\n\
    \            var nCommandsLength = batch[0].length;\n\
    \            for (var nCommand = 0; nCommand != nCommandsLength; nCommand++) {\n\
    \                var cmd = batch[0][nCommand];\n\
    \                if (cmd.Left) {\n\
    \                    var d = cmd.Left;\n\
    \                    switch (d.tag) {\n\
    \                            case \"FreeRef\":\n\
    \                                var refsToFree = jsaddle_free.get(d.contents[0]) || [];\n\
    \                                refsToFree.push(d.contents[1]);\n\
    \                                jsaddle_free.set(d.contents[0], refsToFree);\n\
    \                                break;\n\
    \                            case \"FreeRefs\":\n\
    \                                var refsToFree = jsaddle_free.get(d.contents) || [];\n\
    \                                for(var nRef = 0; nRef != refsToFree.length; nRef++)\n\
    \                                    jsaddle_values.delete(refsToFree[nRef]);\n\
    \                                jsaddle_free.delete(d.contents);\n\
    \                                break;\n\
    \                            case \"SetPropertyByName\":\n\
    \                                jsaddle_values.get(d.contents[0])[d.contents[1]]=jsaddle_values.get(d.contents[2]);\n\
    \                                break;\n\
    \                            case \"SetPropertyAtIndex\":\n\
    \                                jsaddle_values.get(d.contents[0])[d.contents[1]]=jsaddle_values.get(d.contents[2]);\n\
    \                                break;\n\
    \                            case \"EvaluateScript\":\n\
    \                                var n = d.contents[1];\n\
    \                                jsaddle_values.set(n, eval(d.contents[0]));\n\
    \                                break;\n\
    \                            case \"StringToValue\":\n\
    \                                var n = d.contents[1];\n\
    \                                jsaddle_values.set(n, d.contents[0]);\n\
    \                                break;\n\
    \                            case \"GetPropertyByName\":\n\
    \                                var n = d.contents[2];\n\
    \                                jsaddle_values.set(n, jsaddle_values.get(d.contents[0])[d.contents[1]]);\n\
    \                                break;\n\
    \                            case \"GetPropertyAtIndex\":\n\
    \                                var n = d.contents[2];\n\
    \                                jsaddle_values.set(n, jsaddle_values.get(d.contents[0])[d.contents[1]]);\n\
    \                                break;\n\
    \                            case \"NumberToValue\":\n\
    \                                var n = d.contents[1];\n\
    \                                jsaddle_values.set(n, d.contents[0]);\n\
    \                                break;\n\
    \                            case \"NewEmptyObject\":\n\
    \                                var n = d.contents;\n\
    \                                jsaddle_values.set(n, {});\n\
    \                                break;\n\
    \                            case \"NewAsyncCallback\":\n\
    \                                (function() {\n\
    \                                    var nFunction = d.contents;\n\
    \                                    var func = function() {\n\
    \                                        var nFunctionInFunc = ++jsaddle_index;\n\
    \                                        jsaddle_values.set(nFunctionInFunc, func);\n\
    \                                        var nThis = ++jsaddle_index;\n\
    \                                        jsaddle_values.set(nThis, this);\n\
    \                                        var args = [];\n\
    \                                        for (var i = 0; i != arguments.length; i++) {\n\
    \                                            var nArg = ++jsaddle_index;\n\
    \                                            jsaddle_values.set(nArg, arguments[i]);\n\
    \                                            args[i] = nArg;\n\
    \                                        }\n\
    \                                        " <> send "{\"tag\": \"Callback\", \"contents\": [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args]}" <> "\n\
    \                                    };\n\
    \                                    jsaddle_values.set(nFunction, func);\n\
    \                                })();\n\
    \                                break;\n\
    \                            case \"NewSyncCallback\":\n\
    \                                (function() {\n\
    \                                    var nFunction = d.contents;\n\
    \                                    var func = function() {\n\
    \                                        var nFunctionInFunc = ++jsaddle_index;\n\
    \                                        jsaddle_values.set(nFunctionInFunc, func);\n\
    \                                        var nThis = ++jsaddle_index;\n\
    \                                        jsaddle_values.set(nThis, this);\n\
    \                                        var args = [];\n\
    \                                        for (var i = 0; i != arguments.length; i++) {\n\
    \                                            var nArg = ++jsaddle_index;\n\
    \                                            jsaddle_values.set(nArg, arguments[i]);\n\
    \                                            args[i] = nArg;\n\
    \                                        }\n" <> (
    case sendSync of
      Just s  ->
        "                                        if(inCallback > 0) {\n\
        \                                          " <> send "{\"tag\": \"Callback\", \"contents\": [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args]}" <> "\n\
        \                                        } else {\n\
        \                                          runBatch(" <> s "{\"tag\": \"Callback\", \"contents\": [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args]}" <> ", 1);\n\
        \                                        }\n"
      Nothing ->
        "                                        " <> send "{\"tag\": \"Callback\", \"contents\": [lastResults[0], lastResults[1], nFunction, nFunctionInFunc, nThis, args]}" <> "\n"
    ) <>
    "                                    };\n\
    \                                    jsaddle_values.set(nFunction, func);\n\
    \                                })();\n\
    \                                break;\n\
    \                            case \"FreeCallback\":\n\
    \                                callbacksToFree.push(d.contents);\n\
    \                                break;\n\
    \                            case \"CallAsFunction\":\n\
    \                                var n = d.contents[3];\n\
    \                                jsaddle_values.set(n,\n\
    \                                    jsaddle_values.get(d.contents[0]).apply(jsaddle_values.get(d.contents[1]),\n\
    \                                        d.contents[2].map(function(arg){return jsaddle_values.get(arg);})));\n\
    \                                break;\n\
    \                            case \"CallAsConstructor\":\n\
    \                                var n = d.contents[2];\n\
    \                                var r;\n\
    \                                var f = jsaddle_values.get(d.contents[0]);\n\
    \                                var a = d.contents[1].map(function(arg){return jsaddle_values.get(arg);});\n\
    \                                switch(a.length) {\n\
    \                                    case 0 : r = new f(); break;\n\
    \                                    case 1 : r = new f(a[0]); break;\n\
    \                                    case 2 : r = new f(a[0],a[1]); break;\n\
    \                                    case 3 : r = new f(a[0],a[1],a[2]); break;\n\
    \                                    case 4 : r = new f(a[0],a[1],a[2],a[3]); break;\n\
    \                                    case 5 : r = new f(a[0],a[1],a[2],a[3],a[4]); break;\n\
    \                                    case 6 : r = new f(a[0],a[1],a[2],a[3],a[4],a[5]); break;\n\
    \                                    case 7 : r = new f(a[0],a[1],a[2],a[3],a[4],a[5],a[6]); break;\n\
    \                                    default:\n\
    \                                        var ret;\n\
    \                                        var temp = function() {\n\
    \                                            ret = f.apply(this, a);\n\
    \                                        };\n\
    \                                        temp.prototype = f.prototype;\n\
    \                                        var i = new temp();\n\
    \                                        if(ret instanceof Object)\n\
    \                                            r = ret;\n\
    \                                        else {\n\
    \                                            i.constructor = f;\n\
    \                                            r = i;\n\
    \                                        }\n\
    \                                }\n\
    \                                jsaddle_values.set(n, r);\n\
    \                                break;\n\
    \                            case \"NewArray\":\n\
    \                                var n = d.contents[1];\n\
    \                                jsaddle_values.set(n, d.contents[0].map(function(v){return jsaddle_values.get(v);}));\n\
    \                                break;\n\
    \                            case \"SyncWithAnimationFrame\":\n\
    \                                var n = d.contents;\n\
    \                                jsaddle_values.set(n, timestamp);\n\
    \                                break;\n\
    \                            case \"StartSyncBlock\":\n\
    \                                syncDepth++;\n\
    \                                break;\n\
    \                            case \"EndSyncBlock\":\n\
    \                                syncDepth--;\n\
    \                                break;\n\
    \                            default:\n\
    \                                " <> send "{\"tag\": \"ProtocolError\", \"contents\": e.data}" <> "\n\
    \                                return;\n\
    \                    }\n\
    \                } else {\n\
    \                    var d = cmd.Right;\n\
    \                    switch (d.tag) {\n\
    \                            case \"ValueToString\":\n\
    \                                var val = jsaddle_values.get(d.contents);\n\
    \                                var s = val === null ? \"null\" : val === undefined ? \"undefined\" : val.toString();\n\
    \                                results.push({\"tag\": \"ValueToStringResult\", \"contents\": s});\n\
    \                                break;\n\
    \                            case \"ValueToBool\":\n\
    \                                results.push({\"tag\": \"ValueToBoolResult\", \"contents\": jsaddle_values.get(d.contents) ? true : false});\n\
    \                                break;\n\
    \                            case \"ValueToNumber\":\n\
    \                                results.push({\"tag\": \"ValueToNumberResult\", \"contents\": Number(jsaddle_values.get(d.contents))});\n\
    \                                break;\n\
    \                            case \"ValueToJSON\":\n\
    \                                var s = jsaddle_values.get(d.contents) === undefined ? \"\" : JSON.stringify(jsaddle_values.get(d.contents));\n\
    \                                results.push({\"tag\": \"ValueToJSONResult\", \"contents\": s});\n\
    \                                break;\n\
    \                            case \"ValueToJSONValue\":\n\
    \                                results.push({\"tag\": \"ValueToJSONValueResult\", \"contents\": jsaddle_values.get(d.contents)});\n\
    \                                break;\n\
    \                            case \"DeRefVal\":\n\
    \                                var n = d.contents;\n\
    \                                var v = jsaddle_values.get(n);\n\
    \                                var c = (v === null           ) ? [0, \"\"] :\n\
    \                                        (v === undefined      ) ? [1, \"\"] :\n\
    \                                        (v === false          ) ? [2, \"\"] :\n\
    \                                        (v === true           ) ? [3, \"\"] :\n\
    \                                        (typeof v === \"number\") ? [-1, v.toString()] :\n\
    \                                        (typeof v === \"string\") ? [-2, v]\n\
    \                                                                : [n, \"\"];\n\
    \                                results.push({\"tag\": \"DeRefValResult\", \"contents\": c});\n\
    \                                break;\n\
    \                            case \"IsNull\":\n\
    \                                results.push({\"tag\": \"IsNullResult\", \"contents\": jsaddle_values.get(d.contents) === null});\n\
    \                                break;\n\
    \                            case \"IsUndefined\":\n\
    \                                results.push({\"tag\": \"IsUndefinedResult\", \"contents\": jsaddle_values.get(d.contents) === undefined});\n\
    \                                break;\n\
    \                            case \"InstanceOf\":\n\
    \                                results.push({\"tag\": \"InstanceOfResult\", \"contents\": jsaddle_values.get(d.contents[0]) instanceof jsaddle_values.get(d.contents[1])});\n\
    \                                break;\n\
    \                            case \"StrictEqual\":\n\
    \                                results.push({\"tag\": \"StrictEqualResult\", \"contents\": jsaddle_values.get(d.contents[0]) === jsaddle_values.get(d.contents[1])});\n\
    \                                break;\n\
    \                            case \"PropertyNames\":\n\
    \                                var result = [];\n\
    \                                for (name in jsaddle_values.get(d.contents)) { result.push(name); }\n\
    \                                results.push({\"tag\": \"PropertyNamesResult\", \"contents\": result});\n\
    \                                break;\n\
    \                            case \"Sync\":\n\
    \                                results.push({\"tag\": \"SyncResult\", \"contents\": []});\n\
    \                                break;\n\
    \                            default:\n\
    \                                results.push({\"tag\": \"ProtocolError\", \"contents\": e.data});\n\
    \                        }\n\
    \                }\n\
    \            }\n\
    \            if(syncDepth <= 0) {\n\
    \              lastResults = [batch[2], {\"tag\": \"Success\", \"contents\": [callbacksToFree, results]}];\n\
    \              " <> send "{\"tag\": \"BatchResults\", \"contents\": [lastResults[0], lastResults[1]]}" <> "\n\
    \              break;\n\
    \            } else {\n" <> (
    case sendSync of
      Just s  ->
        "              lastResults = [batch[2], {\"tag\": \"Success\", \"contents\": [callbacksToFree, results]}];\n\
        \              batch = " <> s "{\"tag\": \"BatchResults\", \"contents\": [lastResults[0], lastResults[1]]}" <> ";\n\
        \              results = [];\n\
        \              callbacksToFree = [];\n"
      Nothing ->
        "              " <> send "{\"tag\": \"BatchResults\", \"contents\": [batch[2], {\"tag\": \"Success\", \"contents\": [callbacksToFree, results]}]}" <> "\n\
        \              break;\n"
    ) <>
    "            }\n\
    \          } else {\n\
    \            if(syncDepth <= 0) {\n\
    \              break;\n\
    \            } else {\n" <> (
    case sendSync of
      Just s  ->
        "              if(batch[2] === expectedBatch - 1) {\n\
        \                batch = " <> s "{\"tag\": \"BatchResults\", \"contents\": [lastResults[0], lastResults[1]]}" <> ";\n\
        \              } else {\n\
        \                batch = " <> s "{\"tag\": \"Duplicate\", \"contents\": [batch[2], expectedBatch]}" <> ";\n\
        \              }\n\
        \              results = [];\n\
        \              callbacksToFree = [];\n"
      Nothing ->
        "              " <> send "{\"tag\": \"Duplicate\", \"contents\": [batch[2], expectedBatch]}" <> "\n\
        \              break;\n"
    ) <>
    "            }\n\
    \          }\n\
    \        }\n\
    \      }\n\
    \      catch (err) {\n\
    \        var n = ++jsaddle_index;\n\
    \        jsaddle_values.set(n, err);\n\
    \        " <> send "{\"tag\": \"BatchResults\", \"contents\": [batch[2], {\"tag\": \"Failure\", \"contents\": [callbacksToFree, results, n]}]}" <> "\n\
    \      }\n\
    \      if(inCallback == 1) {\n\
    \          while(asyncBatch !== null) {\n\
    \              var b = asyncBatch;\n\
    \              asyncBatch = null;\n\
    \              if(b[2] == expectedBatch) runBatch(b);\n\
    \          }\n\
    \      }\n\
    \      inCallback--;\n\
    \    };\n\
    \    if(batch[1] && (initialSyncDepth || 0) === 0) {\n\
    \        window.requestAnimationFrame(processBatch);\n\
    \    }\n\
    \    else {\n\
    \        processBatch(window.performance ? window.performance.now() : null);\n\
    \    }\n\
    \  };\n\
    \  runBatch(batch);\n\
    \"

-- Use this to generate this string for embedding
-- sed -e 's|\\|\\\\|g' -e 's|^|    \\|' -e 's|$|\\n\\|' -e 's|"|\\"|g' data/jsaddle.js | pbcopy
jsaddleJs :: ByteString
jsaddleJs = "\
    \if(typeof global !== \"undefined\") {\n\
    \    global.window = global;\n\
    \    global.WebSocket = require('ws');\n\
    \}\n\
    \\n\
    \var connect = function() {\n\
    \    var wsaddress =\n\
    \            typeof window.location === \"undefined\"\n\
    \                ? \"ws://localhost:3709/\"\n\
    \                : window.location.protocol.replace('http', 'ws')+\"//\"+window.location.hostname+(window.location.port?(\":\"+window.location.port):\"\");\n\
    \\n\
    \    var ws = new WebSocket(wsaddress);\n\
    \\n\
    \    ws.onopen = function(e) {\n\
    \ " <> initState <> "\
    \\n\
    \        ws.onmessage = function(e) {\n\
    \            var batch = JSON.parse(e.data);\n\
    \\n\
    \ " <> runBatch (\a -> "ws.send(JSON.stringify(" <> a <> "));") Nothing <> "\
    \        };\n\
    \    };\n\
    \    ws.onerror = function() {\n\
    \        setTimeout(connect, 1000);\n\
    \    };\n\
    \}\n\
    \\n\
    \ " <> ghcjsHelpers <> "\
    \connect();\n\
    \"

ghcjsHelpers :: ByteString
ghcjsHelpers = "\
    \function h$isNumber(o) {\
    \    return typeof(o) === 'number';\n\
    \}\n\
    \\n\
    \// returns true for null, but not for functions and host objects\n\
    \function h$isObject(o) {\n\
    \    return typeof(o) === 'object';\n\
    \}\n\
    \\n\
    \function h$isString(o) {\n\
    \    return typeof(o) === 'string';\n\
    \}\n\
    \\n\
    \function h$isSymbol(o) {\n\
    \    return typeof(o) === 'symbol';\n\
    \}\n\
    \\n\
    \function h$isBoolean(o) {\n\
    \    return typeof(o) === 'boolean';\n\
    \}\n\
    \\n\
    \function h$isFunction(o) {\n\
    \    return typeof(o) === 'function';\n\
    \}\n\
    \\n\
    \function h$jsTypeOf(o) {\n\
    \    var t = typeof(o);\n\
    \    if(t === 'undefined') return 0;\n\
    \    if(t === 'object')    return 1;\n\
    \    if(t === 'boolean')   return 2;\n\
    \    if(t === 'number')    return 3;\n\
    \    if(t === 'string')    return 4;\n\
    \    if(t === 'symbol')    return 5;\n\
    \    if(t === 'function')  return 6;\n\
    \    return 7; // other, host object etc\n\
    \}\n\
    \\n\
    \function h$jsonTypeOf(o) {\n\
    \    if (!(o instanceof Object)) {\n\
    \        if (o == null) {\n\
    \            return 0;\n\
    \        } else if (typeof o == 'number') {\n\
    \            if (h$isInteger(o)) {\n\
    \                return 1;\n\
    \            } else {\n\
    \                return 2;\n\
    \            }\n\
    \        } else if (typeof o == 'boolean') {\n\
    \            return 3;\n\
    \        } else {\n\
    \            return 4;\n\
    \        }\n\
    \    } else {\n\
    \        if (Object.prototype.toString.call(o) == '[object Array]') {\n\
    \            // it's an array\n\
    \            return 5;\n\
    \        } else if (!o) {\n\
    \            // null \n\
    \            return 0;\n\
    \        } else {\n\
    \            // it's an object\n\
    \            return 6;\n\
    \        }\n\
    \    }\n\
    \\n\
    \}\n\
    \function h$roundUpToMultipleOf(n,m) {\n\
    \  var rem = n % m;\n\
    \  return rem === 0 ? n : n - rem + m;\n\
    \}\n\
    \\n\
    \function h$newByteArray(len) {\n\
    \  var len0 = Math.max(h$roundUpToMultipleOf(len, 8), 8);\n\
    \  var buf = new ArrayBuffer(len0);\n\
    \  return { buf: buf\n\
    \         , len: len\n\
    \         , i3: new Int32Array(buf)\n\
    \         , u8: new Uint8Array(buf)\n\
    \         , u1: new Uint16Array(buf)\n\
    \         , f3: new Float32Array(buf)\n\
    \         , f6: new Float64Array(buf)\n\
    \         , dv: new DataView(buf)\n\
    \         }\n\
    \}\n\
    \function h$wrapBuffer(buf, unalignedOk, offset, length) {\n\
    \  if(!unalignedOk && offset && offset % 8 !== 0) {\n\
    \    throw (\"h$wrapBuffer: offset not aligned:\" + offset);\n\
    \  }\n\
    \  if(!buf || !(buf instanceof ArrayBuffer))\n\
    \    throw \"h$wrapBuffer: not an ArrayBuffer\"\n\
    \  if(!offset) { offset = 0; }\n\
    \  if(!length || length < 0) { length = buf.byteLength - offset; }\n\
    \  return { buf: buf\n\
    \         , len: length\n\
    \         , i3: (offset%4) ? null : new Int32Array(buf, offset, length >> 2)\n\
    \         , u8: new Uint8Array(buf, offset, length)\n\
    \         , u1: (offset%2) ? null : new Uint16Array(buf, offset, length >> 1)\n\
    \         , f3: (offset%4) ? null : new Float32Array(buf, offset, length >> 2)\n\
    \         , f6: (offset%8) ? null : new Float64Array(buf, offset, length >> 3)\n\
    \         , dv: new DataView(buf, offset, length)\n\
    \         };\n\
    \}\n\
    \function h$newByteArrayFromBase64String(base64) {\n\
    \  var bin = window.atob(base64);\n\
    \  var ba = h$newByteArray(bin.length);\n\
    \  var u8 = ba.u8;\n\
    \  for (var i = 0; i < bin.length; i++) {\n\
    \    u8[i] = bin.charCodeAt(i);\n\
    \  }\n\
    \  return ba;\n\
    \}\n\
    \function h$byteArrayToBase64String(off, len, ba) {\n\
    \  var bin = '';\n\
    \  var u8 = ba.u8;\n\
    \  var end = off + len;\n\
    \  for (var i = off; i < end; i++) {\n\
    \    bin += String.fromCharCode(u8[i]);\n\
    \  }\n\
    \  return window.btoa(bin);\n\
    \}\n\
    \"
