module Datum where

import Data.Map
import Data.Set
import Data.String
import Data.Text
import Database.Persist.MongoDB
import Yesod.Core

type LingvMankoj = Map Text (Set Text)

data Servil =
  Servil
    { akirKonekt :: ConnectionPool
    , akirLingvMank :: LingvMankoj
    }

type Traktil = HandlerFor Servil
