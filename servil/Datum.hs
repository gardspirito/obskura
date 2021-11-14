{-# LANGUAGE MultiParamTypeClasses #-}

module Datum where

import Data.Map ( Map )
import Data.Set ( Set )
import Data.Text ( Text )
import Database.Persist.MongoDB ( ConnectionPool )
import Yesod.Core ( HandlerFor, RenderMessage (..) )
import Network.DNS ( ResolvSeed )
import Yesod.Form ( FormMessage, defaultFormMessage )
import Network.Mail.Mime

type LingvMankoj = Map Text (Set Text)

data Servil =
  Servil
    { akirKonekt :: ConnectionPool
    , akirLingvMank :: LingvMankoj
    , akirDNSSem :: ResolvSeed
    , posxtu :: (Mail -> Mail) -> IO ()
    }

type Traktil = HandlerFor Servil

instance RenderMessage Servil FormMessage where
    renderMessage _ _ = defaultFormMessage