{-# LANGUAGE OverloadedStrings #-}

module Html where


import Prelude hiding (head, id, div)

import Text.Blaze.Html5 hiding (map, main)
import Text.Blaze.Html5.Attributes hiding (title)
import Text.Blaze.Renderer.Utf8

import Data.ByteString.Lazy (ByteString)


genErrPage :: ToMarkup a => a -> ByteString
genErrPage = renderMarkup . errPage

errPage :: ToMarkup a => a -> Markup
errPage message =
  let m = toHtml message
   in html $ do
        head $ title m
        body $ h1 m
