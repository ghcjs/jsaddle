// Single translation unit combining the package's Cocoa Objective-C
// sources.  GHC's runtime linker — used for Template Haskell when the
// compiler is statically linked — refuses the duplicate Objective-C
// protocol/class metadata every ObjC object file emits (e.g.
// __OBJC_LABEL_PROTOCOL_$_NSObject, coalesced by the system linker but
// treated as strong duplicate definitions by GHC), so all of the
// package's ObjC code must land in ONE archive member for TH splices
// in dependents to be able to load this library.
#include "../cbits/WKWebView-cbits.m"
#include "AppDelegate.m"
