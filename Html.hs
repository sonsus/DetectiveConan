{-# LANGUAGE OverloadedStrings #-}

module Html
  ( genErrPage
  , genScoreBoard
  ) where


import Prelude hiding (head, id, div, span)

import Text.Blaze
import Text.Blaze.Html5 hiding (map, main, style)
import Text.Blaze.Html5.Attributes hiding (title, span)
import Text.Blaze.Renderer.Utf8
import Data.ByteString.Lazy (ByteString)
import Control.Monad


genErrPage :: ToMarkup a => a -> ByteString
genErrPage = renderMarkup . errPage

errPage :: ToMarkup a => a -> Markup
errPage message =
  let m = toHtml message
   in html $ do
        head $ title m
        body $ h1 m

genScoreBoard :: (String,String) -> [[String]] -> ByteString
genScoreBoard a b = renderMarkup $ scoreBoard a b

scoreBoard :: (String,String) -> [[String]] -> Html
scoreBoard (t,h) dat = html $ do
  head $ do
    title $ string t
    link ! rel "stylesheet" ! type_ "text/css"  ! href "css.css"
  body . (div ! customAttribute "align" "center") $ do
    p . span $ string h
    table . tbody $ scoreTable dat

scoreTable :: [[String]] -> Html
scoreTable (label:dat) =
  let (scorers,steppers) = splitAt 3 dat
      heading = tableRow label ! class_ "tablelabel"
      top3row = forM_ scorers (\x -> tableRow x ! class_ "hltr")
      leftover= forM_ steppers tableRow
   in heading >> top3row >> leftover

tableRow :: [String] -> Html
tableRow ds = tr $ forM_ ds (td . string)
