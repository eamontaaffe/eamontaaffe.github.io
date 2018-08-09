{-# LANGUAGE OverloadedStrings #-}


module Site (run) where


import           Hakyll
import qualified Aggregate as Aggregate
import qualified About as About


--------------------------------------------------------------------------------

run :: IO ()
run = hakyll $ do
    create ["index.html"] $ do
      route idRoute
      compile hello

    -- match "index.md" $ do
    --   route idRoute

    match "events/*" $
      compile getResourceBody

    create ["about.html"] $ do
      route idRoute
      compile $ Aggregate.compiler About.aggregate


--------------------------------------------------------------------------------

hello :: Compiler (Item String)
hello =
  makeItem "Yo!"
