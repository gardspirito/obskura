module Auxtent where

import qualified Data.Text as T
import Datum
import Network.DNS
import Network.Mail.Mime
import RIO
import Yesod.Core
import Yesod.Form

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
  -- Enmetu
  liftIO $
    posxtu
      servil
      (\malplena ->
         malplena
           { mailTo = [Address {addressName = Nothing, addressEmail = retposxt}]
           , mailParts = [[plainPart "Jes! Ni finfine atingis sukceson!"]]
           })
  respond typePlain ("OK" :: T.Text)
  where
    akirDomajn r
      | [_, dom] <- T.splitOn "@" r = return dom
      | otherwise = invalidArgs ["Retpoŝtadreso ne analizeblas"]
