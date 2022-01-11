module Auxtent where

import Data.ByteString.Base64.URL (encodeBase64)
import Data.Vec.DataFamily.SpineStrict (Vec((:::), VNil))
import Datum
  ( ApiRespond
  , KlientErar(DomajnoNeEkzistasErar, MalgxustaRetposxtErar)
  , Servil(akirDNSSem, akirSalutant, posxtu)
  , Traktil
  , finiFrue
  , klientErar
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
import Yesod.Form (ireq, runInputPost, textField)

cxuServiloEkzist :: ResolvSeed -> Domain -> IO Bool
cxuServiloEkzist sem dom =
  withResolver sem $ \r -> do
    m <- lookupMX r dom
    return $
      case m of
        Right x
          | any ((/= ".") . fst) x -> True
        _ -> False

postAuxtent :: Traktil (ApiRespond ())
postAuxtent = do
  retposxt <- runInputPost (ireq textField "retposxt")
  domajn <- akirDomajn retposxt
  servil <- getYesod
  cxuEkz <- liftIO $ cxuServiloEkzist (akirDNSSem servil) $ encodeUtf8 domajn
  unless cxuEkz (finiFrue <$> klientErar DomajnoNeEkzistasErar)
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
           { mailTo = [Address {addressName = Nothing, addressEmail = retposxt}]
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
      | [_, dom] <- T.split (== '@') r = pure dom
      | otherwise = finiFrue <$> klientErar MalgxustaRetposxtErar
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
