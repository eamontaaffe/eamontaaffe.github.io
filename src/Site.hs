{-# LANGUAGE OverloadedStrings #-}


module Site where


import Control.Monad      (foldM)
import Data.Char          (toLower)
import Data.Foldable      (foldrM)
import Data.List          (intercalate, sortBy)
import Data.List.Split    (splitOn)
import Data.Maybe         (fromMaybe)
import Data.Monoid        ((<>))
import Data.Ord           (compare)
import Hakyll
import Hakyll.Web.Sass    (sassCompiler)
import Network.URI.Encode (encode)
import Text.Read          (readMaybe)
import Text.Regex

-- Exposed
--------------------------------------------------------------------------------

run :: IO ()
run = hakyll $ do
  match "now.json" $ do
    route idRoute
    compile copyFileCompiler
  
  match "templates/*" $
    compile templateBodyCompiler

  scssDependency <- makePatternDependency "css/*.scss"

  rulesExtraDependencies [scssDependency] $ do
    match "css/index.scss" $ do
      route $ setExtension "css"
      compile sassCompiler

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

  tz <- getTimezone

  create ["index.html"] $ do
    route idRoute
    compile $ do
      books   <- recentFirst =<< loadAll "events/*.book.md"
      infos   <- recentFirst =<< loadAll "events/*.info.md"
      snaps   <- recentFirst =<<
        loadAll ("events/*.snap.jpg" .&&. hasVersion "empty")
      travels <- recentFirst =<< loadAll "events/*.travel.md"

      let ctx =
            indexCtx books snaps infos travels

      makeItem ""
        >>= loadAndApplyTemplate "templates/front-page.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" (defaultCtx tz)

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
        >>= loadAndApplyTemplate "templates/default.html" (defaultCtx tz)

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
        >>= loadAndApplyTemplate "templates/default.html" (defaultCtx tz)

  create ["snap.html"] $ do
    route idRoute
    compile $ do
      snaps <- recentFirst
        =<< loadAll ("events/*.snap.jpg" .&&. hasVersion "empty")

      let ctx = snapsCtx snaps

      makeItem ""
        >>= loadAndApplyTemplate "templates/snaps.html" ctx
        >>= loadAndApplyTemplate "templates/with-nav.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" (defaultCtx tz)

  create ["travel.html"] $ do
    route idRoute
    compile $ do
      travels <- recentFirst
        =<< loadAll "events/*.travel.md"

      codes <-
        mapM (\t -> getMetadataField' (itemIdentifier t) "code") travels

      let ctx = travelsCtx travels codes

      makeItem ""
        >>= loadAndApplyTemplate "templates/travel.html" ctx
        >>= loadAndApplyTemplate "templates/with-nav.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" (defaultCtx tz)


-- Internal
--------------------------------------------------------------------------------

defaultCtx :: (String, String) -> Context String
defaultCtx (offset, zone)
  =  constField "offset" (show offset)
  <> constField "zone" zone
  <> defaultContext

travelCtx :: Context String
travelCtx
  =  dateField "date" "%Y.%m.%d"
  <> defaultContext

travelsCtx :: [Item String] -> [String] -> Context String
travelsCtx travels codes
  =  constField "title" "Travel"
  <> field "country-ids" getCountryIds
  <> listField "travels" travelCtx (return travels)
  <> visitedCtx
  <> defaultContext
  where
    getCountryIds _ = do
      codes <-
        mapM (\t -> getMetadataField' (itemIdentifier t) "code") $
          travels :: Compiler [String]

      return . intercalate ", " . map ("#" ++) $ codes

    visitedCtx =
      mconcat $ map (\c -> boolField c (\_-> True)) codes


snapsCtx :: [Item String] -> Context String
snapsCtx snaps
  =  listField "snaps" snapCtx (return snaps)
  <> boolField "snap" (\_-> True)
  <> constField "title" "Snaps"
  <> defaultContext

indexCtx :: [Item String]
         -> [Item String] 
         -> [Item String] 
         -> [Item String]
         -> Context String
indexCtx books snaps infos travels
  =  constField "title" "Eamon Taaffe"
  <> listField "books" eventCtx (return . take 5 $ books)
  <> listField "snaps" snapCtx (return . take 3 $ snaps)
  <> listField "infos" eventCtx (return . take 1 $ infos)
  <> listField "travels" travelCtx (return . take 1 $ travels)
  <> defaultContext


eventCtx :: Context String
eventCtx
  =  dateField "date" "%Y.%m.%d"
  <> field "category" getCategoryField
  <> lowercaseFunctionField
  <> snippetFunctionField
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


snippetFunctionField :: Context String
snippetFunctionField =
  functionField "snippet" fn
  where
    d = "</p>"
    fn xs _ = return . (++ d) . head . splitOn d . unwords $ xs


recentMetadataFirst :: [(Identifier, Metadata)] -> [(Identifier, Metadata)]
recentMetadataFirst xs =
  sortBy ordering xs
  where
    ordering :: (Identifier, Metadata) -> (Identifier, Metadata) -> Ordering
    ordering (_, m1) (_, m2) =
      compare (lookupString "date" m2) (lookupString "date" m1)


getTimezone :: MonadMetadata m => m (String, String)
getTimezone = do
  xs <- getAllMetadata "events/*.travel.md"
  let m      = snd . head . recentMetadataFirst $ xs
  let offset = fromMaybe "10" . lookupString "offset" $ m
  let zone   = fromMaybe "AEST" . lookupString "zone" $ m
  return (offset, zone)
