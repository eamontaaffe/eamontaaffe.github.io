{-# LANGUAGE OverloadedStrings #-}


module Site (run) where


import Hakyll
import Hakyll.Web.Sass (sassCompiler)
import Data.Monoid     ((<>))
import Data.Maybe      (fromMaybe)
import Control.Monad   (foldM)
import Text.Read       (readMaybe)

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
        >>= loadAndApplyTemplate "templates/info.html" ctx
        >>= loadAndApplyTemplate "templates/content.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx

  match "events/*.book.md" $ compile getResourceBody

  create ["read.html"] $ do
    route idRoute
    compile $ do
      books <- recentFirst
        =<< loadAll "events/*.book.md"

      pages <-
        foldM foldPages 0 books

      let ctx =
            booksCtx books pages

      makeItem ""
        >>= loadAndApplyTemplate "templates/read.html" ctx
        >>= loadAndApplyTemplate "templates/content.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx


-- Internal
--------------------------------------------------------------------------------

infoCtx :: [Item String] -> Context String
infoCtx infos@(x:xs)
  =  constField "infoBody" (itemBody x)
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


booksCtx :: [Item String] -> Int -> Context String
booksCtx books@(x:xs) pages
  =  constField "title" "Books"
  <> boolField "read" (\_-> True)
  <> constField "latest-book-title" "Animal Farm"
  <> constField "latest-book-author" "George Orwell"
  <> constField "total-books" (show $ length books)
  <> constField "total-pages" (show pages)
  <> listField "books" bookCtx (return books)
  <> defaultContext


foldPages :: Int -> (Item a) -> Compiler Int
foldPages pages book = do
  field <- getMetadataField (itemIdentifier book) "pages"
  return $ pages + (fromMaybe 0 $ field >>= readMaybe)
