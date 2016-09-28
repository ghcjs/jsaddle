{-# LANGUAGE TemplateHaskell, QuasiQuotes, OverloadedStrings #-}
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

module Language.Javascript.JSaddle.WebSockets.Files (
    mkEmbedded
) where

import WaiAppStatic.Storage.Embedded
import qualified Data.ByteString.Lazy as BL

mkEmbedded :: IO [EmbeddableEntry]
mkEmbedded = do
    indexFile <- BL.readFile "data/index.html"
    jsFile <- BL.readFile "data/jsaddle.js"
    let index = EmbeddableEntry {
                           eLocation = "index.html"
                         , eMimeType = "text/html"
                         , eContent  = Left ("", indexFile)
                         }
        js = EmbeddableEntry {
                           eLocation = "jsaddle.js"
                         , eMimeType = "text/plain"
                         , eContent  = Left ("", jsFile)
                         }
    return [index, js]

