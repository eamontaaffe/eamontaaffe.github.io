{-# LANGUAGE DuplicateRecordFields #-}


module About where


import Aggregate
import Hakyll
import Data.Monoid
import Control.Monad.State (lift)
import Control.Monad.Trans.State (StateT, state)


data State =
  State { edits :: Integer
        , aboutBodyId  :: Maybe Identifier
        }
  deriving (Show)


aggregate :: Aggregate(State)
aggregate =
  Aggregate { reduce = reduceFn
            , initialState = initial
            }


initial :: State
initial =
  State (-1) (Nothing)


reduceFn :: Event -> StateT State Compiler ()
reduceFn Event{type_ = Just "About", identifier = id} =
  state $ \s@State{edits = e} -> ((), s{ edits = e + 1, aboutBodyId = Just id})
reduceFn _ =
  state $ \s -> ((), s)
