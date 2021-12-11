module Datum where

import Control.Monad.Logger as L
import Database.Persist.MongoDB (ConnectionPool)
import Network.DNS (ResolvSeed)
import Network.Mail.Mime
import RIO
import RIO.ByteString (readFile)
import RIO.Text
import Text.JSON
import Text.Printf
import Yesod.Core (HandlerFor, RenderMessage(..))
import Yesod.Form (FormMessage, defaultFormMessage)

type LingvMankoj = HashMap Text (HashSet Text)

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

jsLeg :: (MonadIO m, MonadLogger m, JSON a) => FilePath -> m (Maybe a)
jsLeg dosNomo = do
  d <- readFile dosNomo
  case decode $ unpack $ decodeUtf8With lenientDecode d of
    Ok x -> pure $ Just x
    Error er -> do
      $(L.logError) $
        pack (printf "Error while opening %s: %s" dosNomo er :: String)
      pure Nothing
