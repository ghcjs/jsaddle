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
    \        jsaddle_values.set(0, null);\n\
    \        jsaddle_values.set(1, undefined);\n\
    \        jsaddle_values.set(2, false);\n\
    \        jsaddle_values.set(3, true);\n\
    \        jsaddle_values.set(4, window);\n\
    \        var jsaddle_index = 100;\n\
    \"

runBatch :: (ByteString -> ByteString) -> ByteString
runBatch send = "\
    \            var processBatch = function(timestamp) {\n\
    \                try {\n\
    \                    var nAsyncLength = batch[0].length;\n\
    \                    for (var nAsync = 0; nAsync != nAsyncLength; nAsync++) {\n\
    \                        var d = batch[0][nAsync];\n\
    \                        switch (d.tag) {\n\
    \                            case \"FreeRef\":\n\
    \                                jsaddle_values.delete(d.contents);\n\
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
    \                            case \"NewCallback\":\n\
    \                                (function() {\n\
    \                                    var nFunction = d.contents;\n\
    \                                    jsaddle_values.set(nFunction, function() {\n\
    \                                        var nThis = ++jsaddle_index;\n\
    \                                        jsaddle_values.set(nThis, this);\n\
    \                                        var args = [];\n\
    \                                        for (var i = 0; i != arguments.length; i++) {\n\
    \                                            var nArg = ++jsaddle_index;\n\
    \                                            jsaddle_values.set(nArg, arguments[i]);\n\
    \                                            args[i] = nArg;\n\
    \                                        }\n\
    \                                        " <> send "{\"tag\": \"Callback\", \"contents\": [nFunction, nThis, args]}" <> "\n\
    \                                    })})();\n\
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
    \                                var n = d.contents[0];\n\
    \                                jsaddle_values.set(n, timestamp);\n\
    \                                break;\n\
    \                            default:\n\
    \                                " <> send "{\"tag\": \"ProtocolError\", \"contents\": e.data}" <> "\n\
    \                                return;\n\
    \                        }\n\
    \                    }\n\
    \                    var d = batch[1];\n\
    \                    switch (d.tag) {\n\
    \                        case \"ValueToString\":\n\
    \                            var val = jsaddle_values.get(d.contents);\n\
    \                            var s = val === null ? \"null\" : val === undefined ? \"undefined\" : val.toString();\n\
    \                            " <> send "{\"tag\": \"ValueToStringResult\", \"contents\": s}" <> "\n\
    \                            break;\n\
    \                        case \"ValueToBool\":\n\
    \                            " <> send "{\"tag\": \"ValueToBoolResult\", \"contents\": jsaddle_values.get(d.contents) ? true : false}" <> "\n\
    \                            break;\n\
    \                        case \"ValueToNumber\":\n\
    \                            " <> send "{\"tag\": \"ValueToNumberResult\", \"contents\": Number(jsaddle_values.get(d.contents))}" <> "\n\
    \                            break;\n\
    \                        case \"ValueToJSON\":\n\
    \                            var s = jsaddle_values.get(d.contents) === undefined ? \"\" : JSON.stringify(jsaddle_values.get(d.contents));\n\
    \                            " <> send "{\"tag\": \"ValueToJSONResult\", \"contents\": s}" <> "\n\
    \                            break;\n\
    \                        case \"DeRefVal\":\n\
    \                            var n = d.contents;\n\
    \                            var v = jsaddle_values.get(n);\n\
    \                            var c = (v === null           ) ? [0, \"\"] :\n\
    \                                    (v === undefined      ) ? [1, \"\"] :\n\
    \                                    (v === false          ) ? [2, \"\"] :\n\
    \                                    (v === true           ) ? [3, \"\"] :\n\
    \                                    (typeof v === \"number\") ? [-1, v.toString()] :\n\
    \                                    (typeof v === \"string\") ? [-2, v]\n\
    \                                                            : [n, \"\"];\n\
    \                            " <> send "{\"tag\": \"DeRefValResult\", \"contents\": c}" <> "\n\
    \                            break;\n\
    \                        case \"IsNull\":\n\
    \                            " <> send "{\"tag\": \"IsNullResult\", \"contents\": jsaddle_values.get(d.contents) === null}" <> "\n\
    \                            break;\n\
    \                        case \"IsUndefined\":\n\
    \                            " <> send "{\"tag\": \"IsUndefinedResult\", \"contents\": jsaddle_values.get(d.contents) === undefined}" <> "\n\
    \                            break;\n\
    \                        case \"InstanceOf\":\n\
    \                            " <> send "{\"tag\": \"InstanceOfResult\", \"contents\": jsaddle_values.get(d.contents[0]) instanceof jsaddle_values.get(d.contents[1])}" <> "\n\
    \                            break;\n\
    \                        case \"StrictEqual\":\n\
    \                            " <> send "{\"tag\": \"StrictEqualResult\", \"contents\": jsaddle_values.get(d.contents[0]) === jsaddle_values.get(d.contents[1])}" <> "\n\
    \                            break;\n\
    \                        case \"PropertyNames\":\n\
    \                            var result = [];\n\
    \                            for (name in jsaddle_values.get(d.contents)) { result.push(name); }\n\
    \                            " <> send "{\"tag\": \"PropertyNamesResult\", \"contents\": result}" <> "\n\
    \                            break;\n\
    \                        case \"Sync\":\n\
    \                            " <> send "{\"tag\": \"SyncResult\", \"contents\": []}" <> "\n\
    \                            break;\n\
    \                        default:\n\
    \                            " <> send "{\"tag\": \"ProtocolError\", \"contents\": e.data}" <> "\n\
    \                    }\n\
    \                }\n\
    \                catch (err) {\n\
    \                    var n = ++jsaddle_index;\n\
    \                    jsaddle_values.set(n, err);\n\
    \                    " <> send "{\"tag\": \"ThrowJSValue\", \"contents\": n}" <> "\n\
    \                }\n\
    \            };\n\
    \            if(batch[2]) {\n\
    \                window.requestAnimationFrame(processBatch);\n\
    \            }\n\
    \            else {\n\
    \                processBatch(window.performance ? window.performance.now() : null);\n\
    \            }\n\
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
    \ " <> runBatch (\a -> "ws.send(JSON.stringify(" <> a <> "));") <> "\
    \        };\n\
    \    };\n\
    \    ws.onerror = function() {\n\
    \        setTimeout(connect, 1000);\n\
    \    };\n\
    \}\n\
    \\n\
    \connect();\n\
    \"
