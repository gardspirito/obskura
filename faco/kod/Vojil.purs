module Vojil where

import Prelude hiding ((/))
import Routing.Duplex
import Routing.Duplex.Generic
import Routing.Duplex.Generic.Syntax
import Data.Generic.Rep
import Data.Maybe (Maybe)
import Data.Show
import Data.Show.Generic

data Vojo
  = RegulaPagxo
  | Ensaluti (Maybe String)

instance Show Vojo where
  show = genericShow

derive instance genericVojo :: Generic Vojo _

vojo :: RouteDuplex' Vojo
vojo = root $ sum {
    "RegulaPagxo": noArgs
  , "Ensaluti": "_" / "ensaluti" / (optional segment)
  }
