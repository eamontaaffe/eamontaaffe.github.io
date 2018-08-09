{-# LANGUAGE OverloadedStrings #-}


module Site (run) where


import           Hakyll
import qualified Aggregate as Aggregate
import qualified About as About


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

  create ["about.html"] $ do
    route idRoute
    compile $ Aggregate.compiler About.aggregate


--------------------------------------------------------------------------------

hello :: Compiler (Item String)
hello =
  makeItem "Yo!"
