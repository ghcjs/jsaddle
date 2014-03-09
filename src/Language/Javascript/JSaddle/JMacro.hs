{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}
-----------------------------------------------------------------------------
--
-- Module      :  Language.Javascript.JSaddle.JMacro
-- Copyright   :  (c) Hamish Mackenzie
-- License     :  MIT
--
-- Maintainer  :  Hamish Mackenzie <Hamish.K.Mackenzie@googlemail.com>
--
-- | Helper function to make it easy to embed JMacro in your code and
--   use it from JSaddle
--
-----------------------------------------------------------------------------

module Language.Javascript.JSaddle.JMacro (
    evalJM
  , evalJME
) where

import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Language.Haskell.TH
       (varE, appE, Lit(..), Exp(..), mkName, ExpQ)
import Language.Javascript.JMacro (jmacro, jmacroE, renderJs)

-- | Quasi quoter that creates a JavaScript string from JMacro expression
--   and stores it in a string litteral
evalJME :: QuasiQuoter
evalJME = jmacroE {quoteExp = quoteEvalJME}

quoteEvalJME :: String -> ExpQ
quoteEvalJME s =
    appE (varE 'return) [|AppE (VarE $ mkName "Language.Javascript.JSaddle.eval")
        (LitE . StringL . show $ renderJs $(quoteExp jmacroE s))|]

-- | Quasi quoter that creates a JavaScript string from JMacro statement
--   and stores it in a string litteral
evalJM :: QuasiQuoter
evalJM = jmacro {quoteExp = quoteEvalJM}

quoteEvalJM :: String -> ExpQ
quoteEvalJM s =
    appE (varE 'return) [|AppE (VarE $ mkName "Language.Javascript.JSaddle.eval")
        (LitE . StringL . show $ renderJs $(quoteExp jmacro s))|]
