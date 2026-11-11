# Revision history for jsaddle

## 0.9.10.0

* `Language.Javascript.JSaddle.Int53` (`GHCJS.Int53` on native GHC, from
  ghcjs-base on the JavaScript backend): `Int53` and `Word53`, integers a
  JavaScript Number holds exactly, used by jsaddle-dom 0.10 for Web IDL's
  `long long` / `unsigned long long`.  No `Num` instance on purpose.
* `PToJSVal`, `PFromJSVal`, `ToJSVal` and `FromJSVal` instances for
  `Object` and `SomeJSArray` (`JSArray`, `MutableJSArray`), defined with
  the classes so they are not orphans; `fromJSVal` is `Nothing` for `null`
  and `undefined`.  The orphan `ToJSVal Object` instance in
  `Language.Javascript.JSaddle.Value` is gone.  Packages with their own
  orphan instances for these types need to drop them.
* Requires ghcjs-base >= 0.8.1 on the JavaScript backend.
* `runJavaScriptWithSerializer`: opt-in per-runner batch serialisation.
* Batch results are queued instead of passed through a one-slot `MVar`.
* aeson < 2.4.
