{-# LANGUAGE OverloadedStrings #-}
module Aggregate where


import Hakyll
import Control.Monad.State


compiler :: Compiler (Item String)
compiler =
  undefined

--------------------------------------------------------------------------------

type Ag = String


reduce :: String -> State Ag ()
reduce a = state $ \x -> ((), x ++ a)


final :: State Ag Ag
final = state $ \x -> (x ++ "!!!!!", x)


events :: [String]
events =
  ["Hello ", "world", "!"]


runAgg :: [String] -> Ag -> (String, Ag)
runAgg events =
  runState $ traverse reduce events >> final
