--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
module Site (run) where


--------------------------------------------------------------------------------
import           Hakyll


--------------------------------------------------------------------------------
run :: IO ()
run = hakyll $ do
    create ["index.html"] $ do
      route idRoute
      compile hello

    match "events/*" $
      compile getResourceBody

    create ["about.html"] $ do
      route idRoute
      compile $ compileAggregate about initialAbout


--------------------------------------------------------------------------------
compileAggregate :: (String -> (Item String) -> String)
                 -> String -> Compiler (Item String)
compileAggregate fn x = do
  events <- loadAll "events/*"
  makeItem $ foldl fn x events


--------------------------------------------------------------------------------
hello :: Compiler (Item String)
hello =
  makeItem "Yo!"


--------------------------------------------------------------------------------
about :: String -> (Item String) -> String
about acc x =
  acc ++ itemBody x


initialAbout :: String
initialAbout =
  ""
