{-# LANGUAGE OverloadedStrings #-}


module Site (run) where


import           Hakyll
import           Aggregate (Event, compileEvents, buildContext)
import qualified About as About
import           Data.Monoid


-- Exposed
--------------------------------------------------------------------------------

run :: IO ()
run = hakyll $ do
  match "templates/*" $
    compile templateBodyCompiler

  match "events/*" $
    compile getResourceBody

  create ["pages/index.md"] $ do
    route $ constRoute "index.html"
    compile $ pandocCompiler >>=
        loadAndApplyTemplate "templates/default.html" defaultContext

  create ["pages/about.html"] $ do
    route $ constRoute "about.html"
    compile $ do
      events <- compileEvents "events/*"
      makeItem ""
        >>= loadAndApplyTemplate "templates/about.html" (aboutCtx events)
        >>= loadAndApplyTemplate "templates/default.html" (aboutCtx events)


-- Internal
--------------------------------------------------------------------------------

aboutCtx :: Item [Event] -> Context String
aboutCtx events =
  buildContext About.aggregate events <>
  defaultContext
