{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)


main :: IO ()
main = hakyll $ do
  match "css/*.scss" $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      -- TODO: Need some way to feed the posts list into the template
      -- without causing a circular dependency

      pandocCompiler
        >>= loadAndApplyTemplate "_layouts/post.html" postCtx
        >>= loadAndApplyTemplate "_layouts/default.html" postCtx
        >>= relativizeUrls

    -- match "index.html" $ do
    -- TODO: Load the most recent post into the index.html

    match "_layouts/*" $ compile templateBodyCompiler


siteTitle :: String
siteTitle =
  "Eamon Taaffe"


postCtx :: Context String
postCtx =
  dateField "date" "%B %e, %Y" `mappend`
  constField "siteTitle" siteTitle `mappend`
  defaultContext
