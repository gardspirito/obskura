module Datum where

import Control.Monad.Logger as L
import Database.Persist.MongoDB
import Database.Persist.TH
import Language.Haskell.TH (Type(..))
import Network.DNS (ResolvSeed)
import Network.Mail.Mime
import RIO
import RIO.ByteString (readFile)
import RIO.Text
import Text.JSON
import Text.Printf
import Yesod
import Yesod.Core (HandlerFor, RenderMessage(..))
import Yesod.Form (FormMessage, defaultFormMessage)

type Lingvo = Text

type LingvMankoj = HashMap Lingvo (HashSet Text)

data Servil =
  Servil
    { akirKonekt :: ConnectionPool
    , akirLingvMank :: LingvMankoj
    , akirDNSSem :: ResolvSeed
    , akirSalutant :: TVar (HashMap Text (Maybe Int))
    , posxtu :: Text -> (Mail -> Mail) -> IO ()
    }

type Traktil = HandlerFor Servil

instance RenderMessage Servil FormMessage where
  renderMessage _ _ = defaultFormMessage

instance YesodPersist Servil where
  type YesodPersistBackend Servil = MongoContext
  runDB ago = do
    pool <- akirKonekt <$> getYesod
    runMongoDBPoolDef ago pool

instance MonadFail Traktil where
  fail = error

jsLeg :: (MonadIO m, MonadLogger m, JSON a) => FilePath -> m (Maybe a)
jsLeg dosNomo = do
  d <- readFile dosNomo
  case decode $ unpack $ decodeUtf8With lenientDecode d of
    Ok x -> pure $ Just x
    Error er -> do
      $(L.logError) $
        pack (printf "Error while opening %s: %s" dosNomo er :: String)
      pure Nothing

share
  [mkPersist (mkPersistSettings (ConT ''MongoContext))]
  [persistLowerCase|
Uzant
  retposxt Text
  UnikRetposxt retposxt
|]
