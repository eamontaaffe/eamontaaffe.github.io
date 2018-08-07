{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
module Aggregate where


import Hakyll
import Control.Monad.State
import Data.Functor.Identity (Identity)


compiler :: Compiler (Item String)
compiler =
  evalStateT (build events) initialAbout

--------------------------------------------------------------------------------

data AboutState =
  AboutState { edits :: Integer
             , body  :: String
             }
  deriving (Show)


initialAbout :: AboutState
initialAbout =
  AboutState 0 "There's nothing here..."


data EventType
  = About
  | Run
  deriving (Show)


data Event =
  Event { type_ :: EventType
        , body  :: String
        }
  deriving (Show)


events :: [Event]
events =
  [ Event About "Hi, I'm Eamon!"
  , Event Run "I ran 10.2km today."
  ]


reduce :: Event -> StateT AboutState Compiler ()
reduce (Event {type_=About, body=b}) = state $
  \about@(AboutState {edits=e}) ->
    ((), about { edits=e+1, body=b })
reduce _ = state $
  \s -> ((), s)


final :: StateT AboutState Compiler String
final = state $
  \about@(AboutState { body=b, edits=e }) ->
    (b ++ "\nEdits: " ++ show e, about)


build :: [Event] -> StateT AboutState Compiler (Item String)
build xs = do
  traverse reduce xs
  i <- final
  lift $ makeItem i
  
