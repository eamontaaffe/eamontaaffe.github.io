{-# LANGUAGE OverloadedStrings #-}


module Site (run) where


import           Hakyll
import qualified Aggregate as Aggregate
import qualified About as About
import           Data.Monoid

--------------------------------------------------------------------------------

run :: IO ()
run = hakyll $ do
  match "templates/*" $
    compile templateBodyCompiler

  create ["pages/index.md"] $ do
    route $ constRoute "index.html"
    compile $ pandocCompiler >>=
        loadAndApplyTemplate "templates/default.html" defaultContext

  match "events/*" $
    compile getResourceBody

  create ["pages/about.html"] $ do
    route $ constRoute "about.html"
    compile $
      makeItem ""
        >>= loadAndApplyTemplate "templates/about.html" aboutCtx
        >>= loadAndApplyTemplate "templates/default.html" aboutCtx


--------------------------------------------------------------------------------

aboutCtx :: Context String
aboutCtx
  =  field "about" (\_-> return "Hello world!")
  <> field "edits" (\_-> return "23")
  <> field "title" (\_-> return "About")
  <> defaultContext
