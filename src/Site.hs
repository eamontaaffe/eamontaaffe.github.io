{-# LANGUAGE OverloadedStrings #-}


module Site (run) where


import Hakyll
import Hakyll.Web.Pandoc
import Data.Monoid ((<>))

-- Exposed
--------------------------------------------------------------------------------

run :: IO ()
run = hakyll $ do
  match "templates/*" $
    compile templateBodyCompiler

  create ["pages/index.md"] $ do
    route $ constRoute "index.html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext

  match "events/*.about.md" $ do
    compile $ pandocCompiler
      >>= saveSnapshot "about"

  create ["about.html"] $ do
    route idRoute
    compile $ do
      abouts <-
        loadAllSnapshots "events/*.about.md" "about" :: Compiler [Item String]
      let ctx =
            aboutCtx abouts
      makeItem ""
        >>= loadAndApplyTemplate "templates/about.html" ctx
        >>= loadAndApplyTemplate "templates/default.html" ctx


-- Internal
--------------------------------------------------------------------------------

aboutCtx :: [Item String] -> Context String
aboutCtx abouts
  =  constField "title" "About"
  <> constField "about" (itemBody . last $  abouts)
  <> constField "edits" (show . ((-) 1) . length $ abouts)
  <> defaultContext
