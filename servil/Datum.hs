module Datum where

import Data.Map
import Data.Set
import Data.String
import Data.Text
import Database.Persist.MongoDB
import Yesod.Core

type Mankoj = Map String (Set Text)

data Servil =
  Servil
    { konekt :: ConnectionPool
    , lingvMankoj :: Mankoj
    }

type Traktil = HandlerFor Servil
