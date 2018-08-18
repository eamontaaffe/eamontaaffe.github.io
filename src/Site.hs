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
      abouts <-
        loadAllSnapshots "events/*.info.md" "info"

      let ctx =
            aboutCtx abouts

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

aboutCtx :: [Item String] -> Context String
aboutCtx abouts
  =  constField "content" (itemBody . last $  abouts)
  <> constField "title" "About Eamon Taaffe"
  <> constField "date" "2018.08.16"
  <> constField "edits" (show . ((-) 1) . length $ abouts)
  <> defaultContext


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
