// Haskell callbacks for the jsaddle-wkwebview Objective-C.
//
// These are registered at runtime by the Haskell side
// (jsaddle_wk_set_callbacks, called from
// Language.Javascript.JSaddle.WKWebView.Internal.registerJSaddleCallbacks
// before anything can fire a callback) rather than linked as extern foreign
// exports.  On toolchains where GHCi's RTS linker loads this package's
// archive, the foreign exports are not dyld-visible — and the ObjC, which
// must then be loaded by dyld for its classes to be registered with the
// Objective-C runtime (e.g. compiled into a dylib and preloaded with
// ghci -L<dir> -l<name>; see the objc-in-library cabal flag), could not
// reference them by name.  Function pointers created with
// `foreign import ccall "wrapper"` are callable from both worlds, and
// behave identically in an ordinarily compiled application.
#pragma once
#include "HsFFI.h"
#include <stdbool.h>

typedef struct {
    void (*jsaddleStart)(HsStablePtr);
    void (*jsaddleResult)(HsStablePtr, const char * _Nonnull);
    void (*jsaddleSyncResult)(HsStablePtr, void * _Nonnull handler, const char * _Nonnull);
    void (*callIO)(HsStablePtr);
    void (*callWithCString)(const char * _Nonnull, HsStablePtr);
    bool (*callWithCStringReturningBool)(const char * _Nonnull, HsStablePtr);
    void (*callWithWebView)(void * _Nonnull webView, HsStablePtr);
    void (*callWithCIntCString)(int, const char * _Nonnull, HsStablePtr);
} jsaddle_wk_callbacks;

// Defined in WKWebView-cbits.m (one definition; the iOS build compiles the
// sources as separate translation units, so it cannot be static there).
extern jsaddle_wk_callbacks jsaddleCallbacks;
