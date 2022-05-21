{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}

{-# OPTIONS_GHC -Wno-orphans -Wno-unused-top-binds #-}
import Auxtent (postAuxtent, ensalutTraktil)
import qualified Data.ByteString as ByteString
import Data.Maybe (fromJust)
import Datum (Servil(..))
import Lingvar (getLingvar, legLingv)
import Network.DNS
  ( ResolvConf(resolvTimeout)
  , ResolvSeed
  , defaultResolvConf
  , makeResolvSeed
  )
import Network.Mail.Mime
  ( Address(Address, addressEmail, addressName)
  , Mail(mailHeaders)
  , emptyMail
  , renderMail'
  , sendmailCustom
  )
import RIO
  ( Applicative(pure)
  , Char
  , Eq(..)
  , FilePath
  , Foldable(elem)
  , IO
  , Maybe(Just, Nothing)
  , Monad(return)
  , Read
  , Show
  , Text
  , ($)
  , (&&)
  , (.)
  , (<$>)
  , dropWhile
  , filter
  , not, race_
  )
import qualified RIO.Text as T
import qualified RIO.Text.Partial as TP
import qualified System.FilePath as FilePath
import System.Which (which)
import Yesod
  ( ContentType
  , ErrorResponse(InvalidArgs)
  , PathPiece(..)
  , RenderRoute(renderRoute)
  , Yesod(errorHandler)
  , defaultErrorHandler
  , mkYesod
  , parseRoutes
  , respond
  , sendFile
  , warp
  )
import qualified Yesod.Core.Content as YesCont
import Database.MongoDB (Host, host, connect)
import System.Environment.MrEnv (envAsString)
import MongoKod_

-- FARENDE: Agordaro por retpoŝtadreso de sendanto.
-- FARENDE: Totaleco de IO operacioj (specife de\ readFile, multe uzata en Lingvar)
-- FARENDE: Apriora erartraktilo (por ke klientoj akiru ĝustan JSON kaze de fiasko)
-- FARENDE: Forigi "sesia"
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
/kern/lingvar Lingvar GET
/kern/ensaluti Auxtent POST
!/#DosierPeto DosierP GET
|]

-- /kern/api/konfirmi/#Text Konfirmi POST
instance Yesod Servil where
  errorHandler (InvalidArgs x) = respond YesCont.typePlain $ T.intercalate " " x
  errorHandler aux = defaultErrorHandler aux

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

kreiServil :: Host -> IO Servil
kreiServil dbhost = do
  db <- connect dbhost
  lingvDat <- legLingv
  sem <- kreuDNSSem
  posxtilo <- kreuPosxtilo
  pure $
    Servil
      { akirLingvDat = lingvDat
      , akirDNSSem = sem
      , posxtu = posxtilo
      , akirDb = db
      }

servil :: Host -> IO ()
servil dbhost = do
  serv <- kreiServil dbhost
  race_
    (ensalutTraktil $ akirDb serv)
    (warp 3000 serv)

main :: IO ()
main = do
  dbhost <- host <$> envAsString "DB" "localhost"
  servil dbhost
