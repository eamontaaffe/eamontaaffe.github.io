-- {-# LANGUAGE DuplicateRecordFields #-}


module About where


import Aggregate
import Hakyll (Context, constField)
import Data.Monoid


data State =
  State { edits :: Integer
        , aboutBody  :: String
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
  State (-1) "There's nothing here..."


reduceFn :: State -> Event -> State
reduceFn state@State{ edits = e} Event{ type_ = "About", body = b } =
  state { edits = e + 1
        , aboutBody = b
        }
reduceFn s _ = s


finalFn :: State -> (Context String)
finalFn State{ edits = e, aboutBody = b }
  =  constField "about" b
  <> constField "edits" (show e)
  <> constField "title" "About"
