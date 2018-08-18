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

      booksState <-
        foldM foldBooks initialBooksState books

      let ctx =
            booksCtx books booksState

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


booksCtx :: [Item String] -> BooksState -> Context String
booksCtx books@(x:xs) BooksState{ totalPages   = p
                                , totalBooks   = b
                                , latestAuthor = a
                                , latestTitle  = t
                                }
  =  constField "title" "Books"
  <> boolField "read" (\_-> True)
  <> constField "latest-book-title" (fromMaybe "No Title" t)
  <> constField "latest-book-author" (fromMaybe "No Author" a)
  <> constField "total-books" (show b)
  <> constField "total-pages" (show p)
  <> listField "books" bookCtx (return books)
  <> defaultContext


data BooksState =
  BooksState { totalPages   :: Int
            , totalBooks   :: Int
            , latestTitle  :: Maybe String
            , latestAuthor :: Maybe String
            }

initialBooksState =
  BooksState { totalPages = 0
            , totalBooks = 0
            , latestTitle = Nothing
            , latestAuthor = Nothing
            }

foldBooks :: BooksState -> (Item a) -> Compiler BooksState
foldBooks state@BooksState{ totalPages   = p
                          , totalBooks   = b
                          , latestAuthor = Just _
                          , latestTitle  = Just _
                          } book = do
  field  <- getMetadataField (itemIdentifier book) "pages"
  return $ state { totalPages   = p + (fromMaybe 0 $ field >>= readMaybe)
                 , totalBooks   = b + 1
                 }
foldBooks state@BooksState{ totalPages=p, totalBooks=b } book = do
  field  <- getMetadataField (itemIdentifier book) "pages"
  title  <- getMetadataField (itemIdentifier book) "title"
  author <- getMetadataField (itemIdentifier book) "author"
  return $ state { totalPages   = p + (fromMaybe 0 $ field >>= readMaybe)
                 , totalBooks   = b + 1
                 , latestTitle  = title
                 , latestAuthor = author
                 }
