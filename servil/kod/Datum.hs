module Datum where

import Data.Aeson (decode', defaultOptions, genericToEncoding)
import Data.Aeson.Encoding (unsafeToEncoding)
import Data.Binary.Builder (fromLazyByteString)
import Database.Persist.MongoDB
  ( BackendKey(MongoKey)
  , ConnectionPool
  , MongoContext
  , runMongoDBPoolDef
  )
import Database.Persist.TH
  ( mkPersist
  , mkPersistSettings
  , persistLowerCase
  , share
  )
import Language.Haskell.TH (Type(..))
import Network.DNS (ResolvSeed)
import Network.HTTP.Types (status200, status400, status500)
import Network.Mail.Mime (Mail)
import RIO
  ( FilePath
  , Generic
  , HashMap
  , HashSet
  , IO
  , Int
  , Maybe
  , MonadIO
  , TVar
  , Text
  , Void
  , ($)
  , (<$>)
  , absurd
  , error
  )
import qualified RIO.ByteString.Lazy as BSL
import Yesod
  ( FormMessage
  , FromJSON
  , HandlerFor
  , RenderMessage(..)
  , ToContent(..)
  , ToJSON(toEncoding, toJSON)
  , ToTypedContent(..)
  , YesodPersist(..)
  , defaultFormMessage
  , getYesod
  , sendStatusJSON
  )

type Lingvo = Text

type LingvMankoj = HashMap Lingvo (HashSet Text)

data LingvDatum =
  LingvDatum
    { lingvMankoj :: LingvMankoj
    , lingvSimboloj :: [Text]
    }

data Servil =
  Servil
    { akirKonekt :: ConnectionPool
    , akirLingvDat :: LingvDatum
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

jsLeg :: (MonadIO m, FromJSON a) => FilePath -> m (Maybe a)
jsLeg dosNomo = decode' <$> BSL.readFile dosNomo

share
  [mkPersist (mkPersistSettings (ConT ''MongoContext))]
  [persistLowerCase|
Uzant
  retposxt Text
  UnikRetposxt retposxt
|]

newtype KrudaJson =
  UnsafeKrudaJson BSL.ByteString

instance ToJSON KrudaJson where
  toJSON _ = error "KrudaJson is not supposed to be used this way"
  toEncoding (UnsafeKrudaJson ena) = unsafeToEncoding $ fromLazyByteString ena

newtype ApiRespond v =
  ApiRespond Void

instance ToContent (ApiRespond v) where
  toContent (ApiRespond v) = absurd v

instance ToTypedContent (ApiRespond v) where
  toTypedContent (ApiRespond v) = absurd v

data Respond v
  = Sukc v
  | ServilErar ServilErar
  | KlientErar KlientErar
  deriving (Generic)

data ServilErar =
  TradukErar
  deriving (Generic)

data KlientErar
  = MalgxustaRetposxtErar
  | DomajnoNeEkzistasErar
  | Stub ()
  deriving (Generic)

instance ToJSON ServilErar where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON KlientErar where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON v => ToJSON (Respond v) where
  toEncoding = genericToEncoding defaultOptions

sukc :: ToJSON v => v -> Traktil (ApiRespond v)
sukc v = sendStatusJSON status200 (Sukc v)

servilErar :: ServilErar -> Traktil (ApiRespond v)
servilErar erar = sendStatusJSON status500 (ServilErar erar :: Respond ())

klientErar :: KlientErar -> Traktil (ApiRespond v)
klientErar erar = sendStatusJSON status400 (KlientErar erar :: Respond ())

finiFrue :: ApiRespond a -> b
finiFrue (ApiRespond x) = absurd x
