{-# LANGUAGE OverloadedStrings #-}


module Site where


import Hakyll
import Hakyll.Web.Sass    (sassCompiler)
import Data.Monoid        ((<>))
import Data.Maybe         (fromMaybe)
import Control.Monad      (foldM)
import Text.Read          (readMaybe)
import Network.URI.Encode (encode)
import Text.Regex

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

  match ("events/*.md" .&&. hasNoVersion) $
    compile $
      pandocCompiler
        >>= saveSnapshot "compiled"

  match "events/*.jpg" $ do
    route idRoute
    compile copyFileCompiler

  match "events/*.jpg" $ version "empty" $ do
    compile $ do
      makeItem "" :: Compiler (Item String)

  create ["index.html"] $ do
    route idRoute
    compile $ do
      books <- loadAll "events/*.book.md"
      blogs <- loadAll "events/*.blog.md"
      infos <- loadAll "events/*.info.md"
      snaps <- loadAll ("events/*.snap.jpg" .&&. hasVersion "empty")

      events <-
        recentFirst (books ++ blogs ++ infos ++ snaps)

      let ctx =
            indexCtx events

      makeItem ""
        >>= loadAndApplyTemplate "templates/index.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx

  create ["information.html"] $ do
    route idRoute
    compile $ do
      infos <- recentFirst
        =<< loadAllSnapshots "events/*.info.md" "compiled"

      let ctx =
            infoCtx infos

      makeItem ""
        >>= loadAndApplyTemplate "templates/info.html" ctx
        >>= loadAndApplyTemplate "templates/content.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx

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


indexCtx :: [Item String] -> Context String
indexCtx events
  =  constField "title" "Eamon Taaffe"
  <> listField "events-0" eventCtx (return . take 1 $ events)
  <> listField "events-1" eventCtx (return . take 2 . drop 1 $ events)
  <> listField "events-2" eventCtx (return . take 4 . drop 3 $ events)
  <> listField "events-3" eventCtx (return . drop 7 $ events)
  <> defaultContext


eventCtx :: Context String
eventCtx
  =  dateField "date" "%Y.%m.%d"
  <> field "category" getCategoryField
  <> defaultContext


typeRegex :: Regex
typeRegex =
  mkRegex "\\.([a-z]+)\\.[a-z]+$"


getTypeFromPath :: String -> Maybe String
getTypeFromPath f =
  case (matchRegex typeRegex f) of
    Just ([x]) -> Just x
    _          -> Nothing


getCategoryFromType :: String -> Maybe String
getCategoryFromType "book" = Just "Read"
getCategoryFromType "info" = Just "Information"
getCategoryFromType "snap" = Just "Snap"
getCategoryFromType "blog" = Just "Blog"
getCategoryFromType _      = Nothing


getCategoryFromPath :: String -> Maybe String
getCategoryFromPath p =
  getTypeFromPath p
  >>= getCategoryFromType


getCategoryField :: Item a -> Compiler String
getCategoryField =
  pure . fromMaybe "nope" . getCategoryFromPath . toFilePath . itemIdentifier


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
  <> encodeURIFunctionField
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
foldBooks state@BooksState{ latestAuthor = Just _
                          , latestTitle  = Just _
                          } book = updateBooksCounts state book
foldBooks state@BooksState{ totalPages=p, totalBooks=b } book = do
  state' <- updateBooksCounts state book
  title  <- getMetadataField (itemIdentifier book) "title"
  author <- getMetadataField (itemIdentifier book) "author"
  return $ state' { latestTitle  = title
                  , latestAuthor = author
                  }


updateBooksCounts :: BooksState -> Item a -> Compiler BooksState
updateBooksCounts state@BooksState{ totalPages = p, totalBooks = b } book = do
  field  <- getMetadataField (itemIdentifier book) "pages"
  return $ state { totalPages   = p + (fromMaybe 0 $ field >>= readMaybe)
                 , totalBooks   = b + 1
                 }


encodeURIFunctionField :: Context String
encodeURIFunctionField =
  functionField "encodeURI" fn
  where
    fn xs _ = return . encode . unwords $ xs
