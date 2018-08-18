{-# LANGUAGE OverloadedStrings #-}


module Site (run) where


import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)

-- Exposed
--------------------------------------------------------------------------------

run :: IO ()
run = hakyll $ do
  match "templates/*" $
    compile templateBodyCompiler

  match "css/*.scss" $ do
    route $ setExtension "css"
    let compressCssItem = fmap compressCss
    compile (compressCssItem <$> sassCompiler)

  match "js/*" $ do
    route idRoute
    compile copyFileCompiler

  create ["pages/index.md"] $ do
    route $ constRoute "index.html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "events/*.info.md" $ do
    compile $ pandocCompiler
      >>= saveSnapshot "info"

  create ["information.html"] $ do
    route idRoute
    compile $ do
      infos <- recentFirst
        =<< loadAllSnapshots "events/*.info.md" "info"

      let ctx =
            infoCtx infos

      makeItem ""
        >>= loadAndApplyTemplate "templates/content.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx

  match "events/*.book.md" $ compile getResourceBody

  create ["books.html"] $ do
    route idRoute
    compile $ do
      books <-
        loadAll "events/*.book.md"

      let ctx =
            booksCtx books

      makeItem ""
        >>= loadAndApplyTemplate "templates/books.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx


-- Internal
--------------------------------------------------------------------------------

infoCtx :: [Item String] -> Context String
infoCtx infos@(x:xs)
  =  constField "content" (itemBody x)
  <> boolField "info" (\_-> True)
  <> field "title" getTitle
  <> constField "date" "2018.08.16"
  <> constField "edits" (show . (subtract 1) . length $ infos)
  <> defaultContext
  where
    getTitle _ = do
      field <- getMetadataField (itemIdentifier x) "title"
      return $ fromMaybe "No title" field


bookCtx :: Context String
bookCtx
  =  dateField "date" "%B %e, %Y"
  <> defaultContext


booksCtx :: [Item String] -> Context String
booksCtx books
  =  constField "title" "Books"
  -- TODO: Calculate total pages from metadata
  <> constField "total-pages" "101"
  <> listField "books" bookCtx (return books)
  <> constField "total" (show . length $ books)
  <> defaultContext
