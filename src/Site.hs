{-# LANGUAGE OverloadedStrings #-}


module Site (run) where


import           Hakyll
import           Hakyll.Web.Pandoc
import           Data.Maybe (fromJust)

import           Aggregate (buildEvents, compileAggregate)
import qualified About as A
import qualified Books as B

-- Exposed
--------------------------------------------------------------------------------

run :: IO ()
run = hakyll $ do
  match "templates/*" $
    compile templateBodyCompiler

  match "events/*.md" $ do
    compile $ pandocCompiler
      >>= saveSnapshot "events"

  events <-
    buildEvents "events/*"

  create ["pages/index.md"] $ do
    route $ constRoute "index.html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  create ["about.html"] $ do
    route idRoute
    compile $ do
      about@(A.State{A.aboutBodyId=b, A.edits=e}) <-
        compileAggregate A.aggregate events

      body <- loadSnapshotBody (fromJust b) "events" :: Compiler (String)

      let aboutCtx =
            constField "edits" (show e) `mappend`
            bodyField "about"

      let defaultCtx =
            constField "title" "About" `mappend`
            bodyField "body"

      makeItem body
        >>= loadAndApplyTemplate "templates/about.html" aboutCtx
        >>= loadAndApplyTemplate "templates/default.html" defaultCtx

  create ["books.html"] $ do
    route idRoute
    compile $ do
      books <- compileAggregate B.aggregate events

      let booksCtx =
            constField "total" (show $ length books) `mappend`
            listFieldWith "books" defaultContext (return books)

      let defaultCtx =
            constField "title" "Books" `mappend`
            bodyField "body"

      makeItem ""
        >>= loadAndApplyTemplate "templates/books.html" booksCtx
        >>= loadAndApplyTemplate "templates/default.html" defaultCtx
