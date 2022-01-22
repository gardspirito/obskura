{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}

module Auxtent where

import Data.Aeson (FromJSON)
import Data.ByteString.Base64.URL (encodeBase64)
import Data.Vec.DataFamily.SpineStrict (Vec((:::), VNil))
import Datum
  ( ApiRespond
  , KlientErar(DomajnoNeEkzistasErar, MalgxustaRetposxtErar)
  , Servil(akirDNSSem, akirSalutant, posxtu)
  , Traktil
  , akirPet
  , auFrue
  , klientErar
  , malpurRaporti
  , sukc
  )
import Lingvar (kajtpet, tKuntDe, tpet, tpetPartoj, traduki)
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
  , Eq((/=), (==))
  , Generic
  , IO
  , Int
  , Maybe(..)
  , Monad(return)
  , Text
  , ($)
  , (.)
  , (<$>)
  , any
  , atomically
  , encodeUtf8
  , fst
  , maybe
  , otherwise
  , readTVar
  , unless
  , writeTVar
  )
import qualified RIO.HashMap as HM
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import System.Random.Stateful (applyAtomicGen, genByteString, globalStdGen)
import Yesod.Core (MonadIO(..), getYesod)

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

postAuxtent :: Traktil (ApiRespond ())
postAuxtent = do
  pet <- auFrue akirPet
  let uzRetposxt = retposxt pet
  domajn <-
    maybe
      (malpurRaporti <$> klientErar DomajnoNeEkzistasErar)
      pure
      (akirDomajn uzRetposxt)
  servil <- getYesod
  cxuEkz <- liftIO $ cxuServiloEkzist (akirDNSSem servil) $ encodeUtf8 domajn
  unless cxuEkz (malpurRaporti <$> klientErar MalgxustaRetposxtErar)
  kod <- registriSalut servil Nothing
  (tsal, (tsubj, t1 ::: t2 ::: t3 ::: t4 ::: VNil)) <-
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
                     , t1
                     , "\n\n"
                     , t2
                     , ": http://localhost:3000/kern/ensalutkod/"
                     , kod
                     , "\n"
                     , t3
                     , "\n\n"
                     , t4
                     ]
                 ]
               ]
           })
  sukc ()
  where
    akirDomajn r
      | [_, dom] <- T.split (== '@') r = Just dom
      | otherwise = Nothing
    registriSalut :: MonadIO m => Servil -> Maybe Int -> m Text
    registriSalut (akirSalutant -> salutant) iden = registri'
      where
        registri' :: MonadIO m => m Text
        registri' = do
          krudKod <- applyAtomicGen (genByteString 60) globalStdGen
          let kod = encodeBase64 krudKod
          sukcEnskribite <- liftIO $ atomically $ enskribi kod
          if sukcEnskribite
            then pure kod
            else registri'
        enskribi kod = do
          sal' <- readTVar salutant
          case HM.lookup kod sal' of
            Nothing -> do
              writeTVar salutant $ HM.insert kod iden sal'
              pure True
            Just _ -> pure False
