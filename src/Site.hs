{-# LANGUAGE OverloadedStrings #-}


module Site where


import Hakyll
import Hakyll.Web.Sass    (sassCompiler)
import Data.Monoid        ((<>))
import Data.Maybe         (fromMaybe)
import Data.Char          (toLower)
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

      let ctx =
            indexCtx books blogs snaps infos

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
        >>= loadAndApplyTemplate "templates/with-nav.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx

  create ["read.html"] $ do
    route idRoute
    compile $ do
      books <- recentFirst
        =<< loadAll "events/*.book.md"

      booksState <-
        foldM foldBooks initialBooksState books

      let ctx = booksCtx books booksState

      makeItem ""
        >>= loadAndApplyTemplate "templates/read.html" ctx
        >>= loadAndApplyTemplate "templates/with-nav.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx

  create ["snap.html"] $ do
    route idRoute
    compile $ do
      snaps <- recentFirst
        =<< loadAll ("events/*.snap.jpg" .&&. hasVersion "empty")

      let ctx = snapsCtx snaps

      makeItem ""
        >>= loadAndApplyTemplate "templates/snaps.html" ctx
        >>= loadAndApplyTemplate "templates/with-nav.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx

  create ["travel.html"] $ do
    route idRoute
    compile $ do
      makeItem ""
        >>= loadAndApplyTemplate "templates/travel.html" defaultContext
        >>= loadAndApplyTemplate "templates/with-nav.html" defaultContext
        >>= loadAndApplyTemplate "templates/default.html" defaultContext


-- Internal
--------------------------------------------------------------------------------


snapsCtx :: [Item String] -> Context String
snapsCtx snaps
  =  listField "snaps" snapCtx (return snaps)
  <> boolField "snap" (\_-> True)
  <> constField "title" "Snaps"
  <> defaultContext

indexCtx :: [Item String] -> [Item String] -> [Item String] -> [Item String]
         -> Context String
indexCtx books blogs snaps infos
  =  constField "title" "Eamon Taaffe"
  <> listField "books" eventCtx (return books)
  <> listField "blogs" eventCtx (return blogs)
  <> listField "snaps" snapCtx (return snaps)
  <> listField "infos" eventCtx (return infos)
  <> defaultContext


eventCtx :: Context String
eventCtx
  =  dateField "date" "%Y.%m.%d"
  <> field "category" getCategoryField
  <> lowercaseFunctionField
  <> defaultContext

snapCtx :: Context String
snapCtx
  =  dateField "date" "%Y.%m.%d"
  <> field "img" (pure . toFilePath . itemIdentifier)
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


lowercaseFunctionField :: Context String
lowercaseFunctionField =
  functionField "lowercase" fn
  where
    fn xs _ = return . unwords . map (map toLower) $ xs
