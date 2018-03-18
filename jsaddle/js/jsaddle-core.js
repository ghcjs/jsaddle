function jsaddle(global, sendRsp, startSyncCallback, continueSyncCallback) {
  /*

  Queue.js

  A function to represent a queue

  Created by Kate Morley - http://code.iamkate.com/ - and released under the terms
  of the CC0 1.0 Universal legal code:

  http://creativecommons.org/publicdomain/zero/1.0/legalcode

  */

  /* Creates a new queue. A queue is a first-in-first-out (FIFO) data structure -
   * items are added to the end of the queue and removed from the front.
   */
  function Queue(){

    // initialise the queue and offset
    var queue  = [];
    var offset = 0;

    // Returns the length of the queue.
    this.getLength = function(){
      return (queue.length - offset);
    }

    // Returns true if the queue is empty, and false otherwise.
    this.isEmpty = function(){
      return (queue.length == 0);
    }

    /* Enqueues the specified item. The parameter is:
     *
     * item - the item to enqueue
     */
    this.enqueue = function(item){
      queue.push(item);
    }

    /* Enqueues the specified items; faster than calling 'enqueue'
     * repeatedly. The parameter is:
     *
     * items - an array of items to enqueue
     */
    this.enqueueArray = function(items){
      queue.push.apply(queue, items);
    }

    /* Dequeues an item and returns it. If the queue is empty, the value
     * 'undefined' is returned.
     */
    this.dequeue = function(){

      // if the queue is empty, return immediately
      if (queue.length == 0) return undefined;

      // store the item at the front of the queue
      var item = queue[offset];

      // increment the offset and remove the free space if necessary
      if (++ offset * 2 >= queue.length){
        queue  = queue.slice(offset);
        offset = 0;
      }

      // return the dequeued item
      return item;

    }

    /* Returns the item at the front of the queue (without dequeuing it). If the
     * queue is empty then undefined is returned.
     */
    this.peek = function(){
      return (queue.length > 0 ? queue[offset] : undefined);
    }

  }
  /* End Queue.js */

  var vals = new Map();
  vals.set(1, global);
  var nextValId = -1;
  var unwrapVal = function(valId) {
    if(typeof valId === 'object') {
      if(valId === null) {
        return null;
      } else if(valId.length === 0) {
        return undefined;
      } else {
        return vals.get(valId[0]);
      }
    } else {
      return valId;
    }
  };
  var wrapVal = function(val, def) { //TODO: Check that 'def' is never a problem here - does Array.map pass a second argument?
    switch(typeof val) {
    case 'undefined':
      return [];

    case 'boolean':
    case 'number':
    case 'string':
      return val;

    case 'object':
      if(val === null) {
        return null;
      }
      // Fall through
    default:
      if(def) {
        return [def];
      }
      var valId = nextValId--;
      vals.set(valId, val);
      return [valId];
    }
  };
  var result = function(ref, val) {
    vals.set(ref, val);
    sendRsp({
      'tag': 'Result',
      'contents': [
        ref,
        wrapVal(val, [])
      ]
    });
  };
  var syncRequests = new Queue();
  var getNextSyncRequest = function() {
    if(syncRequests.isEmpty()) {
      syncRequests.enqueueArray(continueSyncCallback());
    }
    return syncRequests.dequeue();
  }
  var processAllEnqueuedReqs = function() {
    while(!syncRequests.isEmpty()) {
      var req = syncRequests.dequeue();
      if(!req.Right) {
        throw "processAllEnqueuedReqs: req is not Right; this should never happen because Lefts should only be sent while a synchronous request is still in progress";
      }
      processSingleReq(req.Right);
    }
  };
  var syncDepth = 0;
  var runSyncCallback = function(callback, that, args) {
    syncDepth++;
    syncRequests.enqueueArray(startSyncCallback(callback, that, args));
    while(true) {
      var rsp = getNextSyncRequest();
      if(rsp.Right) {
        processSingleReq(rsp.Right);
      } else {
        syncDepth--;
        if(syncDepth === 0 && !syncRequests.isEmpty()) {
          // Ensure that all remaining sync requests are cleared out in a timely
          // fashion.  Any incoming websocket requests will also run
          // processAllEnqueuedReqs, but it could potentially be an unlimited
          // amount of time before the next websocket request comes in.  We
          // can't process this synchronously because we need to return right
          // now - it's possible the next item in the queue will make use of
          // something we were supposed to produce, so if we run that without
          // returning first, it won't be available
          setTimeout(processAllEnqueuedReqs, 0);
        }
        return rsp.Left;
      }
    }
  };
  var processSingleReq = function(req) {
    switch(req.tag) {
    case 'FreeRef':
      vals.delete(req.contents[0]);
      break;
    case 'NewJson':
      result(req.contents[1], req.contents[0]);
      break;
    case 'GetJson':
      sendRsp({
        'tag': 'GetJson',
        'contents': [
          req.contents[1],
          unwrapVal(req.contents[0])
        ]
      });
      break;
    case 'SyncBlock':
      runSyncCallback(req.contents[0], [], []);
      break;
    case 'NewSyncCallback':
      result(req.contents[1], function() {
        var result = runSyncCallback(req.contents[0], wrapVal(this), Array.prototype.slice.call(arguments).map(wrapVal));
        if(result.Left) {
          throw unwrapVal(result.Left);
        }
        return unwrapVal(result.Right);
      });
      break;
    case 'NewAsyncCallback':
      var callbackId = req.contents[0];
      result(req.contents[1], function() {
        sendRsp({
          'tag': 'CallAsync',
          'contents': [
            callbackId,
            wrapVal(this),
            Array.prototype.slice.call(arguments).map(wrapVal)
          ]
        });
      });
      break;
    case 'SetProperty':
      unwrapVal(req.contents[2])[unwrapVal(req.contents[0])] = unwrapVal(req.contents[1]);
      break;
    case 'GetProperty':
      result(req.contents[2], unwrapVal(req.contents[1])[unwrapVal(req.contents[0])]);
      break;
    case 'CallAsFunction':
      result(req.contents[3], unwrapVal(req.contents[0]).apply(unwrapVal(req.contents[1]), req.contents[2].map(unwrapVal)));
      break;
    case 'CallAsConstructor':
      result(req.contents[2], new (Function.prototype.bind.apply(unwrapVal(req.contents[0]), [null].concat(req.contents[1].map(unwrapVal)))));
      break;
    default:
      throw 'processSingleReq: unknown request tag ' + JSON.stringify(req.tag);
    }
  };
  var processReq = function(req) {
    processAllEnqueuedReqs();
    processSingleReq(req);
  };
  return {
    processReq: processReq
  };
}
