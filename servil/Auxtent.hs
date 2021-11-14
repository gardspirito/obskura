{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Auxtent where

import Control.Monad
import qualified Data.ByteString.Char8 as BS
import qualified Data.Text as T
import Data.Text.Encoding
import Datum
import Network.DNS
import Network.DNS.Lookup
import Network.Mail.Mime
import Yesod.Core
import Yesod.Form

cxuRetposxtEkzist :: ResolvSeed -> Domain -> IO Bool
cxuRetposxtEkzist sem dom =
  withResolver sem $ \r -> do
    m <- lookupMX r dom
    let cxuValida x = any ((/= ".") . fst) x
    return $ either (const False) cxuValida m

postAuxtent :: Traktil TypedContent
postAuxtent = do
  retposxt <- runInputPost (ireq textField "retposxt")
  domajn <- akirDomajn retposxt
  servil <- getYesod
  cxuEkz <- liftIO $ cxuRetposxtEkzist (akirDNSSem servil) $ encodeUtf8 domajn
  unless cxuEkz (invalidArgs ["RETPOSXT_NE_EKZISTAS"])
    -- FARENDA: Enmetu
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
