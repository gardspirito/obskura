module Auxtent where

import Datum
import Network.DNS
import Network.Mail.Mime
import RIO
import qualified RIO.HashMap as HM
import qualified RIO.Text as T
import qualified RIO.Text.Lazy as TL
import Yesod.Core
import Yesod.Form

--import Control.Concurrent.STM
import Data.ByteString.Base64.URL
import System.Random
import System.Random.Stateful

cxuServiloEkzist :: ResolvSeed -> Domain -> IO Bool
cxuServiloEkzist sem dom =
  withResolver sem $ \r -> do
    m <- lookupMX r dom
    return $
      case m of
        Right x
          | any ((/= ".") . fst) x -> True
        _ -> False

postAuxtent :: Traktil TypedContent
postAuxtent = do
  retposxt <- runInputPost (ireq textField "retposxt")
  domajn <- akirDomajn retposxt
  servil <- getYesod
  cxuEkz <- liftIO $ cxuServiloEkzist (akirDNSSem servil) $ encodeUtf8 domajn
  unless cxuEkz (invalidArgs ["SERVILO_NE_EKZISTAS"])
  kod <- registriSalut servil Nothing
  -- Enmetu
  liftIO $
    posxtu
      servil
      "Test"
      (\malplena ->
         malplena
           { mailTo = [Address {addressName = Nothing, addressEmail = retposxt}]
           , mailParts =
               [ [ plainPart $
                   TL.concat
                     ["Jes! Ni finfine atingis sukceson! ", TL.fromStrict kod]
                 ]
               ]
           })
  respond typePlain ("OK" :: T.Text)
  where
    akirDomajn r
      | [_, dom] <- T.split (== '@') r = return dom
      | otherwise = invalidArgs ["Retpoŝtadreso ne analizeblas"]
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
