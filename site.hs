--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll
import           Data.Binary
import           Data.Typeable


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    create ["index.html"] $ do
      route idRoute
      compile hello

    match "events/*" $
      compile getResourceBody

    create ["about.html"] $ do
      route idRoute
      compile $ compileAggregate about initialAbout


--------------------------------------------------------------------------------
compileAggregate :: (Typeable b, Binary b)
                 => (a -> (Item b) -> a) -> a -> Compiler (Item a)
compileAggregate fn x = do
  events <- loadAll "events/*"
  makeItem $ foldl fn x events

-- foldM :: (Foldable t, Monad m) => (b -> a -> m b) -> b -> t a -> m b 

--------------------------------------------------------------------------------
hello :: Compiler (Item String)
hello =
  makeItem "Yo!"


--------------------------------------------------------------------------------
about :: String -> (Item String) -> String
about acc x =
  acc ++ itemBody x


-- thing :: (Item String) -> Maybe String
-- thing a = do
--   Maybe.fromMaybe "Nope!" $ getMetadataField (getUnderlying a) "title"

initialAbout :: String
initialAbout =
  ""
