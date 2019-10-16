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
        compile $ pandocCompiler
            >>= loadAndApplyTemplate "_layouts/post.html"    postCtx
            >>= loadAndApplyTemplate "_layouts/default.html" postCtx
            >>= relativizeUrls

    -- match "index.html" $ do
    --     route idRoute
    --     compile $ do
    --         posts <- recentFirst =<< loadAll "posts/*"
    --         let indexCtx =
    --                 listField "posts" postCtx (return posts) `mappend`
    --                 constField "title" "Home"                `mappend`
    --                 defaultContext

    --         getResourceBody
    --             >>= applyAsTemplate indexCtx
    --             >>= loadAndApplyTemplate "_layouts/default.html" indexCtx
    --             >>= relativizeUrls

    match "_layouts/*" $ compile templateBodyCompiler


siteTitle :: String
siteTitle =
  "Eamon Taaffe"


postCtx :: Context String
postCtx =
    dateField "date" "%B %e, %Y" `mappend`
    constField "siteTitle" siteTitle `mappend`
    defaultContext
