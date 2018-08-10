module Books where


import Aggregate
import Hakyll
import Control.Monad.State (lift)
import Control.Monad.Trans.State (StateT, state)
import Data.Maybe (fromMaybe)


type State =
  [Book]


data Book =
  Book { title  :: Maybe String
       , author :: Maybe String
       , date   :: Maybe String
       }


aggregate :: Aggregate(State)
aggregate =
  Aggregate { reduce = reduce'
            , initialState = initialState'
            }


initialState' :: State
initialState' =
  []


reduce' :: Event -> StateT State Compiler ()
reduce' Event{type_ = Just "Book", identifier = id} = do
  author <- lift $ getMetadataField id "author"
  title <- lift $ getMetadataField id "title"
  date <- lift $ getMetadataField id "date"
  let book' =
        Book { title = title
             , author = author
             , date = date
             }
  state $ \books -> ((), book':books)
reduce' _ =
  state $ \s -> ((), s)
