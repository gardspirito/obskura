{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}

import Auxtent
import qualified Data.ByteString as ByteString
import Data.Maybe
import Database.MongoDB.Connection
import Database.Persist.MongoDB
import Database.Persist.TH
import Datum
import Language.Haskell.TH (Type(..))
import Lingvar
import Network.DNS
import Network.Mail.Mime
import RIO hiding (Handler)
import qualified RIO.HashMap as HM
import qualified RIO.Text as T
import qualified RIO.Text.Partial as TP
import qualified System.FilePath as FilePath
import System.Which
import Yesod
import qualified Yesod.Core.Content as YesCont

-- FARENDE: Agordaro por retpoŝtadreso de sendanto.
-- FARENDE: Malesperantigi erarojn
data DosierPeto =
  DosierPeto
    { dosPlenNomo :: !Text
    , dosFin :: !Text
    }
  deriving (Eq, Show, Read)

instance PathPiece DosierPeto where
  toPathPiece = dosPlenNomo
  fromPathPiece x
    | not $ T.null x =
      let (kom, fin) = TP.breakOnEnd "." x
       in if T.null kom
            then Nothing
            else Just $ DosierPeto x fin
  fromPathPiece _ = Nothing

mkYesod
  "Servil"
  [parseRoutes|
/lingvar Lingvar GET
/api/ensalutu Auxtent POST
!/#DosierPeto DosierP GET
|]

instance Yesod Servil where
  errorHandler (InvalidArgs x) = respond YesCont.typePlain $ T.intercalate " " x
  errorHandler aux = defaultErrorHandler aux

getLingvar :: Handler TypedContent
getLingvar = lingvar

-- /#DosierPeto DosierP GET
sufAlTip :: DosierPeto -> ContentType
sufAlTip = f' . dosFin
  where
    f' "html" = YesCont.typeHtml
    f' x
      | x `elem` ["jpeg", "jpg"] = YesCont.typeJpeg
    f' "png" = YesCont.typePng
    f' "gif" = YesCont.typeGif
    f' "svg" = YesCont.typeSvg
    f' "js" = YesCont.typeJavascript
    f' "css" = YesCont.typeCss
    f' "json" = YesCont.typeJson
    f' x
      | x `elem` ["oft", "ttf", "woff", "woff2"] =
        ByteString.concat ["font/", T.encodeUtf8 x]
    f' _ = YesCont.typeOctet

statVoj :: [Char] -> FilePath
statVoj =
  FilePath.joinPath .
  ("stat/" :) .
  filter purF . FilePath.splitPath . FilePath.normalise . dropWhile (== '/')
  where
    purF x = x /= "../" && x /= "./"

getDosierP :: DosierPeto -> Handler ()
getDosierP peto =
  sendFile (sufAlTip peto) $ statVoj $ T.unpack $ dosPlenNomo peto

kreuDNSSem :: IO ResolvSeed
kreuDNSSem = makeResolvSeed defaultResolvConf {resolvTimeout = 1000000}

-- Krei funkcion, kiu, akirinte plenigilon de retmesaĝon per datumo (ĉio krom informo pri adresanto),
-- sendas retmesaĝon.
kreuPosxtilo :: IO (Text -> (Mail -> Mail) -> IO ())
kreuPosxtilo = do
  vojAlSm <- fromJust <$> which "sendmail" -- Trovi vojon al sendmail aplikaĵo
  return $ \subjekto datumPlenigilo -> do
    let senTema =
          datumPlenigilo $
          emptyMail $
          Address
            { addressName = Just "Obskurativ"
            , addressEmail = "obs@dev.obscurative.ru"
            }
    let msg =
          senTema {mailHeaders = ("Subject", subjekto) : mailHeaders senTema}
    finmsg <- renderMail' msg
    sendmailCustom vojAlSm ["-t"] finmsg

kreiServil :: IO Servil
kreiServil = do
  mongo <-
    createMongoDBPool
      "obskurativ"
      "localhost"
      defaultPort
      Nothing
      defaultPoolStripes
      defaultStripeConnections
      defaultConnectionIdleTime
  mankoj <- legMankojn
  sem <- kreuDNSSem
  posxtilo <- kreuPosxtilo
  salutant <- newTVarIO HM.empty
  pure $
    Servil
      { akirKonekt = mongo
      , akirLingvMank = mankoj
      , akirDNSSem = sem
      , akirSalutant = salutant
      , posxtu = posxtilo
      }

main :: IO ()
main = kreiServil >>= warp 3000
