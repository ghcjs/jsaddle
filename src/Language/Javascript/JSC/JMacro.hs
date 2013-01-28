{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSC.JMacro
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Helper function to make it easy to embed JMacro in your code and
--   use it from JSC
--
-----------------------------------------------------------------------------

module Language.Javascript.JSC.JMacro (
    evalJM
) where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH
       (varE, appE, Lit(..), Exp(..), mkName, ExpQ)
import Language.Javascript.JMacro (jmacroE, renderJs)

evalJM :: QuasiQuoter
evalJM = jmacroE {quoteExp = quoteEvalJM}

quoteEvalJM :: String -> ExpQ
quoteEvalJM s =
    appE (varE 'return) [|AppE (VarE $ mkName "Language.Javascript.JSC.eval")
        (LitE . StringL . show $ renderJs $(quoteExp jmacroE s))|]
