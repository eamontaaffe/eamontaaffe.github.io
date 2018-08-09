{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Aggregate where


import Hakyll
import Control.Monad.State
import Data.Maybe


-- Exposed
--------------------------------------------------------------------------------


compileEvents :: Pattern -> Compiler (Item [Event])
compileEvents p = do
  items <- (loadAll p) :: Compiler [Item String]
  metadatas <- getAllMetadata $ p :: Compiler [(Identifier, Metadata)]
  makeItem $ map unpackEvent (zip items metadatas)


buildContext
  :: Aggregate(a)
  -> Item [Event]
  -> Context String
buildContext Aggregate{ reduce=r, final=f, initialState=i } events =
  evalState (traverse (handle r) (itemBody events) >> (finish f) ) i


data Event =
  Event { type_ :: String
        , body  :: String
        }
  deriving (Show)


data Aggregate(a) =
  Aggregate { reduce       :: a -> Event -> a
            , final        :: a -> Context String
            , initialState :: a
            }


-- Internal
--------------------------------------------------------------------------------

handle
  :: (a -> Event -> a)
  -> Event
  -> State a ()
handle fn event =
  state $ \s -> ((), fn s event)


finish :: (a -> (Context String)) -> State a (Context String)
finish fn =
  state $ \s -> (fn s, s)


unpackEvent :: (Item String, (Identifier, Metadata)) -> Event
unpackEvent (item, (id, meta)) =
  Event { type_ = unpackEventType meta, body = itemBody item }


unpackEventType :: Metadata -> String
unpackEventType m =
  fromMaybe "undefined" (lookupString "type" m)
