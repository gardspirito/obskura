{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Auxtent where

import Data.Aeson (FromJSON)
import Data.Bson
    ( look,
      Binary(..),
      Field((:=)),
      ObjectId,
      Value(Float, String, Int32) )
import qualified Data.ByteString.Base64.URL as Base64.URL
import qualified Data.ByteString.Base64 as Base64
import Data.Time.Clock
    ( UTCTime, secondsToNominalDiffTime, addUTCTime, getCurrentTime )
import Data.Vec.DataFamily.SpineStrict (Vec((:::), VNil))
import Database.MongoDB ( runCommand, Pipe )
import Datum
  ( ApiRespond
  , KlientErar(DomajnoNeEkzistasErar, MalgxustaRetposxtErar)
  , Servil(akirDb, akirDNSSem, posxtu)
  , Traktil
  , akirPet
  , auFrue
  , klientErar
  , malpurRaporti
  , sukc
  )
import Datumbaz
    ( Retposxt,
      Konservita(..),
      H,
      H',
      Enmet,
      AId,
      Filtrita,
      Filtr(kontroli),
      m_,
      (<.),
      en,
      en',
      enmet,
      forigCxiuj,
      rw, Elekt )
import Lingvar (kajtpet, tKuntDe, tpet, tpetPartoj, traduki)
import MongoKod
import Network.DNS (Domain, ResolvSeed, lookupMX, withResolver)
import Network.Mail.Mime
  ( Address(Address, addressEmail, addressName)
  , Mail(mailParts, mailTo)
  , plainPart
  )
import RIO
  ( Applicative(pure)
  , Bool(..)
  , Either(Right)
  , Eq((/=))
  , Generic
  , IO
  , Int
  , Maybe(..)
  , Monad(return)
  , Text
  , ($)
  , (*)
  , (.)
  , (<$>)
  , (>>=)
  , any
  , encodeUtf8
  , forever
  , fst
  , maybe
  , threadDelay
  , unless
  , traceIO
  )
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import System.Random.Stateful (applyAtomicGen, genByteString, globalStdGen)
import Yesod.Core (MonadIO(..), getYesod)

data UzantId v
  = EkzUzant !(H v ObjectId '[]) -- FARENDE: Neredaktebla!
  | NovUzant !(H v (Filtrita Retposxt) '[]) -- Ni konservu retpoŝton por posta uzo
  deriving (Generic)
instance MongoKod (UzantId v)
instance MongoMalkod (UzantId Elekt)

data Ensalut v =
  Ensalut
    { ensalutUzant :: !(H' v UzantId '[AId])
    , ensalutKod :: !(H v Text '[])
    , petintToken :: !(H v Text '[])
    , ensalutGxis :: !(H v UTCTime '[])
    }
  deriving (Generic)

instance MongoKod (Ensalut v)

--instance MongoKod (Ensalut v)
instance Konservita Ensalut where
  konservejo = "ensalut"

-- | Ensalutito
data AktivUzant v =
  AktivUzant
    { uzantToken :: !(H v Binary '[AId])
    , uzantId :: !(H v ObjectId '[])
    }

instance Konservita AktivUzant where
  konservejo = "aktiv-uzant"

sek :: Int
sek = 1000000

-- | Demono, kreanta kolekto por ensalutantoj kaj certiganta ĝian purigon.
ensalutTraktil :: Pipe -> IO a
ensalutTraktil kon =
  rw
    kon
    do _ <- runCommand ["drop" := String "ensalut"]
       Float 1 <-
         runCommand
           [ "create" := String "ensalut"
           , "capped" := String "true"
           , "size" := Int32 8192
           ] >>=
         look "ok"
       forever $ do
         --traceIO "cikl"
         liftIO $ threadDelay (300 * sek)
         nun <- liftIO getCurrentTime
         forigCxiuj $ m_ {ensalutGxis = (<.) nun}

cxuServiloEkzist :: ResolvSeed -> Domain -> IO Bool
cxuServiloEkzist sem dom =
  withResolver sem $ \r -> do
    m <- lookupMX r dom
    return $
      case m of
        Right x
          | any ((/= ".") . fst) x -> True
        _ -> False

newtype AuxtentPet =
  AuxtentPet
    { retposxt :: Text
    }
  deriving (Generic)

instance FromJSON AuxtentPet

postAuxtent :: Traktil (ApiRespond Text)
postAuxtent = do
  pet <- auFrue akirPet
  let uzRetposxt = retposxt pet
  fretposxt@(_, retDom) <-
    maybe
      (malpurRaporti <$> klientErar DomajnoNeEkzistasErar)
      pure
      (kontroli @Retposxt uzRetposxt)
  servil <- getYesod
  cxuEkz <- liftIO $ cxuServiloEkzist (akirDNSSem servil) $ encodeUtf8 retDom
  unless cxuEkz (malpurRaporti <$> klientErar MalgxustaRetposxtErar)
  (kod, jxeton) <- registriSalut (NovUzant $ en fretposxt) -- Farende!
  (tsal, (tsubj, t0 ::: t1 ::: t2 ::: t3 ::: VNil)) <-
    traduki $
    tKuntDe "servil.retmsg" $
    tpet "salut" `kajtpet` tKuntDe "ensaluti" (tpet "temo" `kajtpet` tpetPartoj)
  -- Enmetu
  liftIO $
    posxtu
      servil
      tsubj
      (\malplena ->
         malplena
           { mailTo =
               [Address {addressName = Nothing, addressEmail = uzRetposxt}]
           , mailParts =
               [ [ plainPart $
                   TL.fromStrict $
                   T.concat
                     [ tsal
                     , " "
                     , t0
                     , "\n\n"
                     , t1
                     , ": http://localhost:3000/kern/ensalutkod/"
                     , kod
                     , "\n"
                     , t2
                     , "\n\n"
                     , t3
                     ]
                 ]
               ]
           })
  sukc jxeton
  where
    -- | Registri ensaluton.
    -- | Rezultigas paron (Konfirmkodo, jxetono)
    registriSalut :: UzantId Enmet -> Traktil (Text, Text)
    registriSalut iden = do
        kod <- Base64.URL.encodeBase64 <$> applyAtomicGen (genByteString 60) globalStdGen
        jxeton <- Base64.encodeBase64 <$> applyAtomicGen (genByteString 120) globalStdGen
        db <- akirDb <$> getYesod
        nun <- liftIO getCurrentTime
        _ <- rw db $
          enmet $ Ensalut {
            ensalutUzant = en' iden,
            ensalutKod = en kod,
            petintToken = en jxeton,
            ensalutGxis = en $ addUTCTime (secondsToNominalDiffTime 7 * 60) nun
          }
        pure (kod, jxeton)
        
