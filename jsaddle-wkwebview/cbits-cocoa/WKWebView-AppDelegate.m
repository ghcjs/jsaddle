// Single translation unit combining the package's Cocoa Objective-C
// sources.  GHC's runtime linker — used for Template Haskell when the
// compiler is statically linked — refuses the duplicate Objective-C
// protocol/class metadata every ObjC object file emits (e.g.
// __OBJC_LABEL_PROTOCOL_$_NSObject, coalesced by the system linker but
// treated as strong duplicate definitions by GHC), so all of the
// package's ObjC code must land in ONE archive member for TH splices
// in dependents to be able to load this library.
// Combining the sources puts the whole translation unit into clang's
// nullability-audit mode (AppDelegate.m uses _Nonnull), which then
// demands annotations on every pointer in WKWebView-cbits.m — the
// files compile cleanly standalone, so silence the completeness
// diagnostics for the composite.
#pragma clang diagnostic ignored "-Wnullability-completeness"
#pragma clang diagnostic ignored "-Wnullability-completeness-on-arrays"
#include "../cbits/WKWebView-cbits.m"
#include "AppDelegate.m"
