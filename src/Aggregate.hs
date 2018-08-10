{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}


module Aggregate where


import           Hakyll
import           Hakyll.Core.Rules (rulesExtraDependencies)
import           Control.Monad.Trans.State
import           Data.Maybe
import qualified Data.Set as Set


-- Types
--------------------------------------------------------------------------------

data Events =
  Events { events     :: [Event]
         , dependency :: Dependency
         }

data Event =
  Event { type_      :: Maybe String 
        , identifier :: Identifier -- The identifier can be used to get the body
        }
  deriving (Show)


data Aggregate(a) =
  Aggregate { reduce       :: Event -> StateT a Compiler ()
            , initialState :: a
            }


-- Exposed
--------------------------------------------------------------------------------


buildEvents :: MonadMetadata m => Pattern -> m Events
buildEvents p = do
  ids <- getMatches p
  events' <- mapM getEvent ids
  return Events
    { events = events'
    , dependency = PatternDependency p (Set.fromList ids)
    }


getEvent :: MonadMetadata m => Identifier -> m Event
getEvent id = do
  metadata <- getMetadata id
  let type_ = lookupString "type" metadata
  return Event
    { identifier = id
    , type_ = type_
    }


compileAggregate :: Aggregate a -> Events -> Compiler a
compileAggregate Aggregate{reduce=r, initialState=i} Events{events=e} =
  execStateT (traverse r e) i
