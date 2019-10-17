{-# LANGUAGE OverloadedStrings #-}

import Data.Monoid (mappend)
import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Data.Maybe (fromMaybe)


main :: IO ()
main = hakyll $ do
  match "css/*.scss" $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

  match "posts/*" $ version "id" $ do
    compile $ makeItem ("" :: String)

  match "posts/*" $ do
    route $ setExtension "html"
    compile $ do
      postTitles <-
        groupByYear
        =<< recentFirst
        =<< loadAll ("posts/*" .&&. hasVersion "id")

      let archiveCtx =
            dateField "date" "%B %e, %Y" `mappend`
            constField "siteTitle" siteTitle `mappend`
            listField "posts" postCtx (return postTitles) `mappend`
            defaultContext

      pandocCompiler
        >>= loadAndApplyTemplate "_layouts/post.html" archiveCtx
        >>= loadAndApplyTemplate "_layouts/default.html" archiveCtx
        >>= relativizeUrls

    -- match "index.html" $ do
    -- TODO: Load the most recent post into the index.html

    match "_layouts/*" $ compile templateBodyCompiler


siteTitle :: String
siteTitle =
  "Eamon Taaffe"

postCtx :: Context String
postCtx =
  dateField "year" "%Y" `mappend`
  dateField "date" "%e %B" `mappend`
  constField "siteTitle" siteTitle `mappend`
  defaultContext

groupByYear :: MonadMetadata m => [Item a] -> m [Item [a]]
groupByYear =
  groupByM $ getYear . itemIdentifier
  where
    groupByM :: (Monad m) =>
                (Item a -> m String) -> [Item a] -> m [Item [a]]
    groupByM =
      undefined -- TODO

    getYear :: MonadMetadata m => Identifier -> m String
    getYear id = do
      metadata <- getMetadata id
      return (fromMaybe undefined . lookupString "year" $ metadata)
