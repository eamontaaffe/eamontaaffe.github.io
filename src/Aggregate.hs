{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Aggregate where


import Hakyll
import Control.Monad.State
import Data.Maybe


compiler :: Aggregate(a) -> Compiler (Item String)
compiler Aggregate{ reduce=r, final=f, initialState=i } =
  evalStateT (build r f) i


--------------------------------------------------------------------------------

data Aggregate(a) =
  Aggregate { reduce      :: a -> Event -> a
            , final       :: a -> String
            , initialState :: a
            }

data Event =
  Event { type_ :: String
        , body  :: String
        }
  deriving (Show)

build :: (a -> Event -> a) -> (a -> String) -> StateT a Compiler (Item String)
build rn fn = do
  items     <- lift . loadAll $ "events/*"
  metadatas <- lift . getAllMetadata $ "events/*"
  aboutItem <- traverse (handle rn) (zip items metadatas) >> (finish fn)
  lift . makeItem $ aboutItem


handle :: (a -> Event -> a) -> ((Item String), (Identifier, Metadata))
       -> StateT a Compiler ()
handle fn (i,(_, m)) =
  state $ \s -> ((), fn s event)
  where
    event =
      Event { type_ = unpackEventType m
            , body  = itemBody i
            }

unpackEventType :: Metadata -> String
unpackEventType m =
  fromMaybe "undefined" (lookupString "type" m)

finish :: (a -> String) -> StateT a Compiler String
finish fn =
  state $ \s -> (fn s, s)


--------------------------------------------------------------------------------
