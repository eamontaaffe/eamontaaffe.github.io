{-# LANGUAGE DuplicateRecordFields #-}
module About where

import Aggregate

data State =
  State { edits :: Integer
        , body  :: String
        }
  deriving (Show)


aggregate :: Aggregate(State)
aggregate =
  Aggregate { reduce = reduceFn
            , final = finalFn
            , initialState = initial
            }

initial :: State
initial =
  State 0 "There's nothing here..."


reduceFn :: State -> Event -> State
reduceFn state@State{ edits = e} Event{ type_ = "About", body = b } =
  state { edits = e + 1
        , body = b
        }
reduceFn s _ = s

finalFn :: State -> String
finalFn State{ edits = e, body = b } =
  b ++ "\n\nTotal edits: " ++ show e
