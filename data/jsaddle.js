if(typeof global !== "undefined") {
    global.window = global;
    global.WebSocket = require('ws');
}

var connect = function() {
    var wsaddress =
            typeof window.location === "undefined"
                ? "ws://localhost:3709/"
                : window.location.protocol.replace('http', 'ws')+"//"+window.location.hostname+(window.location.port?(":"+window.location.port):"");

    var ws = new WebSocket(wsaddress);

    ws.onopen = function(e) {
        var jsaddle_values = new Map();
        jsaddle_values.set(0, null);
        jsaddle_values.set(1, undefined);
        jsaddle_values.set(2, false);
        jsaddle_values.set(3, true);
        jsaddle_values.set(4, window);
        var jsaddle_index = 100;

        ws.onmessage = function(e) {
            try {
                var batch = JSON.parse(e.data);
                var nAsyncLength = batch[0].length;
                for (var nAsync = 0; nAsync != nAsyncLength; nAsync++) {
                    var d = batch[0][nAsync];
                    switch (d.tag) {
                        case "FreeRef":
                            jsaddle_values.delete(d.contents);
                            break;
                        case "SetPropertyByName":
                            jsaddle_values.get(d.contents[0])[d.contents[1]]=jsaddle_values.get(d.contents[2]);
                            break;
                        case "SetPropertyAtIndex":
                            jsaddle_values.get(d.contents[0])[d.contents[1]]=jsaddle_values.get(d.contents[2]);
                            break;
                    }
                }
                var d = batch[1];
                switch (d.tag) {
                    case "EvaluateScript":
                        var n = ++jsaddle_index;
                        jsaddle_values.set(n, eval(d.contents));
                        ws.send(JSON.stringify({"tag": "EvaluateScriptResult", "contents": n}));
                        break;
                    case "ValueToString":
                        var val = jsaddle_values.get(d.contents);
                        var s = val === null ? "null" : val === undefined ? "undefined" : val.toString();
                        ws.send(JSON.stringify({"tag": "ValueToStringResult", "contents": s}));
                        break;
                    case "StringToValue":
                        var n = ++jsaddle_index;
                        jsaddle_values.set(n, d.contents);
                        ws.send(JSON.stringify({"tag": "StringToValueResult", "contents": n}));
                        break;
                    case "GetPropertyByName":
                        var n = ++jsaddle_index;
                        jsaddle_values.set(n, jsaddle_values.get(d.contents[0])[d.contents[1]]);
                        ws.send(JSON.stringify({"tag": "GetPropertyByNameResult", "contents": n}));
                        break;
                    case "GetPropertyAtIndex":
                        var n = ++jsaddle_index;
                        jsaddle_values.set(n, jsaddle_values.get(d.contents[0])[d.contents[1]]);
                        ws.send(JSON.stringify({"tag": "GetPropertyAtIndexResult", "contents": n}));
                        break;
                    case "NumberToValue":
                        var n = ++jsaddle_index;
                        jsaddle_values.set(n, d.contents);
                        ws.send(JSON.stringify({"tag": "NumberToValueResult", "contents": n}));
                        break;
                    case "NewEmptyObject":
                        var n = ++jsaddle_index;
                        jsaddle_values.set(n, {});
                        ws.send(JSON.stringify({"tag": "NewEmptyObjectResult", "contents": n}));
                        break;
                    case "NewCallback":
                        var nFunction = ++jsaddle_index;
                        jsaddle_values.set(nFunction, function() {
                            var nThis = ++jsaddle_index;
                            jsaddle_values.set(nThis, this);
                            var args = [];
                            for (var i = 0; i != arguments.length; i++) {
                                var nArg = ++jsaddle_index;
                                jsaddle_values.set(nArg, arguments[i]);
                                args[i] = nArg;
                            }
                            ws.send(JSON.stringify({"tag": "Callback", "contents": [nFunction, nThis, args]})); });
                        ws.send(JSON.stringify({"tag": "NewCallbackResult", "contents": nFunction}));
                        break;
                    case "CallAsFunction":
                        var n = ++jsaddle_index;
                        jsaddle_values.set(n,
                            jsaddle_values.get(d.contents[0]).apply(jsaddle_values.get(d.contents[1]),
                                d.contents[2].map(function(arg){return jsaddle_values.get(arg);})));
                        ws.send(JSON.stringify({"tag": "CallAsFunctionResult", "contents": n}));
                        break;
                    case "CallAsConstructor":
                        var n = ++jsaddle_index;
                        var r;
                        var f = jsaddle_values.get(d.contents[0]);
                        var a = d.contents[1].map(function(arg){return jsaddle_values.get(arg);});
                        switch(a.length) {
                            case 0 : r = new f(); break;
                            case 1 : r = new f(a[0]); break;
                            case 2 : r = new f(a[0],a[1]); break;
                            case 3 : r = new f(a[0],a[1],a[2]); break;
                            case 4 : r = new f(a[0],a[1],a[2],a[3]); break;
                            case 5 : r = new f(a[0],a[1],a[2],a[3],a[4]); break;
                            case 6 : r = new f(a[0],a[1],a[2],a[3],a[4],a[5]); break;
                            case 7 : r = new f(a[0],a[1],a[2],a[3],a[4],a[5],a[6]); break;
                            default:
                                var ret;
                                var temp = function() {
                                    ret = f.apply(this, a);
                                };
                                temp.prototype = f.prototype;
                                var i = new temp();
                                if(ret instanceof Object)
                                    r = ret;
                                else {
                                    i.constructor = f;
                                    r = i;
                                }
                        }
                        jsaddle_values.set(n, r);
                        ws.send(JSON.stringify({"tag": "CallAsConstructorResult", "contents": n}));
                        break;
                    case "NewArray":
                        var n = ++jsaddle_index;
                        jsaddle_values.set(n, d.contents.map(function(v){return jsaddle_values.get(v);}));
                        ws.send(JSON.stringify({"tag": "NewArrayResult", "contents": n}));
                        break;
                    case "ValueToBool":
                        ws.send(JSON.stringify({"tag": "ValueToBoolResult", "contents": jsaddle_values.get(d.contents) ? true : false}));
                        break;
                    case "ValueToNumber":
                        ws.send(JSON.stringify({"tag": "ValueToNumberResult", "contents": Number(jsaddle_values.get(d.contents))}));
                        break;
                    case "ValueToJSON":
                        var s = jsaddle_values.get(d.contents) === undefined ? "" : JSON.stringify(jsaddle_values.get(d.contents));
                        ws.send(JSON.stringify({"tag": "ValueToJSONResult", "contents": s}));
                        break;
                    case "DeRefVal":
                        var n = d.contents;
                        var v = jsaddle_values.get(n);
                        var c = (v === null           ) ? [0, ""] :
                                (v === undefined      ) ? [1, ""] :
                                (v === false          ) ? [2, ""] :
                                (v === true           ) ? [3, ""] :
                                (typeof v === "number") ? [-1, v.toString()] :
                                (typeof v === "string") ? [-2, v]
                                                        : [n, ""];
                        ws.send(JSON.stringify({"tag": "DeRefValResult", "contents": c}));
                        break;
                    case "IsNull":
                        ws.send(JSON.stringify({"tag": "IsNullResult", "contents": jsaddle_values.get(d.contents) === null}));
                        break;
                    case "IsUndefined":
                        ws.send(JSON.stringify({"tag": "IsUndefinedResult", "contents": jsaddle_values.get(d.contents) === undefined}));
                        break;
                    case "InstanceOf":
                        ws.send(JSON.stringify({"tag": "InstanceOfResult", "contents": jsaddle_values.get(d.contents[0]) instanceof jsaddle_values.get(d.contents[1])}));
                        break;
                    case "StrictEqual":
                        ws.send(JSON.stringify({"tag": "StrictEqualResult", "contents": jsaddle_values.get(d.contents[0]) === jsaddle_values.get(d.contents[1])}));
                        break;
                    case "PropertyNames":
                        var result = [];
                        for (name in jsaddle_values.get(d.contents)) { result.push(name); }
                        ws.send(JSON.stringify({"tag": "PropertyNamesResult", "contents": result}));
                        break;
                    case "Sync":
                        ws.send(JSON.stringify({"tag": "SyncResult"}));
                        break;
                    default:
                        ws.send(JSON.stringify({"tag": "ProtocolError", "contents": e.data}));
                }
            }
            catch (err) {
                var n = ++jsaddle_index;
                jsaddle_values.set(n, err);
                ws.send(JSON.stringify({"tag": "ThrowJSValue", "contents": n}));
            }
        };
    };
    ws.onerror = function() {
        setTimeout(connect, 1000);
    };
}

connect();
