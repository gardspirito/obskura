module Datum where

import Data.Aeson (FromJSON, ToJSON, decode', defaultOptions, genericToEncoding)
import qualified Data.Aeson as Aeson
import Data.Aeson.Encoding (unsafeToEncoding)
import Data.Binary.Builder (fromLazyByteString)
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
  , Maybe
  , MonadIO
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
  , defaultFormMessage
  , parseCheckJsonBody
  , sendStatusJSON
  )
import Database.MongoDB (Pipe)

type Lingvo = Text

type LingvMankoj = HashMap Lingvo (HashSet Text)

data LingvDatum =
  LingvDatum
    { lingvMankoj :: !LingvMankoj
    , lingvSimboloj :: ![Text]
    }

data Servil =
  Servil
    { akirLingvDat :: !LingvDatum
    , akirDNSSem :: !ResolvSeed
    , posxtu :: !(Text -> (Mail -> Mail) -> IO ())
    , akirDb :: !Pipe
    }

type Traktil = HandlerFor Servil

instance RenderMessage Servil FormMessage where
  renderMessage _ _ = defaultFormMessage

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

-- | Fini pritraktado de peto frue okaze de eraro.
auFrue :: Traktil (Either (ApiRespond a) a) -> Traktil a
auFrue f = f <&> either malpurRaporti id

-- | Malkodigi JSON peto de kliento.
akirPet :: FromJSON a => Traktil (Either (ApiRespond erar) a)
akirPet =
  parseCheckJsonBody >>= \case
    Aeson.Success x -> pure $ Right x
    Aeson.Error erar -> klientErar (PetErar $ T.pack erar) <&> Left

data Respond v
  = Sukc !v
  | ServilErar
  | KlientErar KlientErar
  deriving (Generic)

data KlientErar
  = MalgxustaRetposxtErar
  | DomajnoNeEkzistasErar
  | PetErar !Text
  | ErarojAnkauxPovasHaviKorpon_ ()
  deriving (Generic)