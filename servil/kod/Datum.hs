module Datum where

import Data.Aeson (FromJSON, ToJSON, decode', defaultOptions, genericToEncoding)
import qualified Data.Aeson as Aeson
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
  ( Either(..)
  , FilePath
  , Generic
  , HashMap
  , HashSet
  , IO
  , Int
  , Maybe
  , MonadIO
  , TVar
  , Void
  , ($)
  , (.)
  , (<$>)
  , (<&>)
  , (>>=)
  , absurd
  , either
  , error
  , id
  )
import qualified RIO.ByteString.Lazy as BSL
import RIO.Prelude (pure)
import qualified RIO.Text as T
import RIO.Text
import Yesod
  ( FormMessage
  , HandlerFor
  , RenderMessage(..)
  , ToContent(..)
  , ToJSON(toEncoding, toJSON)
  , ToTypedContent(..)
  , YesodPersist(..)
  , defaultFormMessage
  , getYesod
  , parseCheckJsonBody
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

newtype KrudaJson =
  UnsafeKrudaJson BSL.ByteString

unsafeKrudaRespond :: BSL.ByteString -> Traktil (ApiRespond v)
unsafeKrudaRespond = sendStatusJSON status200 . UnsafeKrudaJson

instance ToJSON KrudaJson where
  toJSON _ = error "KrudaJson is not supposed to be used this way"
  toEncoding (UnsafeKrudaJson ena) = unsafeToEncoding $ fromLazyByteString ena

newtype ApiRespond v =
  ApiRespond Void

instance ToContent (ApiRespond v) where
  toContent (ApiRespond v) = absurd v

instance ToTypedContent (ApiRespond v) where
  toTypedContent (ApiRespond v) = absurd v

instance ToJSON KlientErar where
  toEncoding = genericToEncoding defaultOptions

instance ToJSON v => ToJSON (Respond v) where
  toEncoding = genericToEncoding defaultOptions

sukc :: ToJSON v => v -> Traktil (ApiRespond v)
sukc v = sendStatusJSON status200 (Sukc v)

servilErar :: Traktil (ApiRespond v)
servilErar = sendStatusJSON status500 (ServilErar :: Respond ())

klientErar :: KlientErar -> Traktil (ApiRespond v)
klientErar erar = sendStatusJSON status400 (KlientErar erar :: Respond ())

malpurRaporti :: ApiRespond a -> b
malpurRaporti (ApiRespond v) = absurd v

auFrue :: Traktil (Either (ApiRespond a) a) -> Traktil a
auFrue f = f <&> either malpurRaporti id

akirPet :: FromJSON a => Traktil (Either (ApiRespond erar) a)
akirPet =
  parseCheckJsonBody >>= \case
    Aeson.Success x -> pure $ Right x
    Aeson.Error erar -> klientErar (PetErar $ T.pack erar) <&> Left

data Respond v
  = Sukc v
  | ServilErar
  | KlientErar KlientErar
  deriving (Generic)

data KlientErar
  = MalgxustaRetposxtErar
  | DomajnoNeEkzistasErar
  | PetErar Text
  | ErarojAnkauxPovasHaviKorpon_ ()
  deriving (Generic)

share
  [mkPersist (mkPersistSettings (ConT ''MongoContext))]
  [persistLowerCase|
Uzant
  retposxt Text
  UnikRetposxt retposxt
|]
