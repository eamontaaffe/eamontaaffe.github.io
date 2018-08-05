--------------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
import           Data.Monoid (mappend)
import           Hakyll


--------------------------------------------------------------------------------
main :: IO ()
main = hakyll $ do
    create ["index.html"] $ do
      route idRoute
      compile hello

--------------------------------------------------------------------------------

hello :: Compiler (Item String)
hello =
  makeItem "Yo"
