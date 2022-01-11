module Datum
  ( Erar(..)
  , HHTML
  , KlientErar(..)
  , Tradukil
  , ServilErar(..)
  , Tradukenda(..)
  , fapl
  , fdevas
  , fen
  , fperm
  , mapErar
  , petKern
  , skrKErar
  , setigi
  , striktAlfabet
  ) where

import Safe.Coerce
import Affjax as Affj
import Affjax.RequestBody as Affj.Pet
import Affjax.ResponseFormat as Affj.Resp
import Data.Argonaut.Core (Json)
import Data.Argonaut.Decode (class DecodeJson, JsonDecodeError, decodeJson)
import Data.Argonaut.Encode (class EncodeJson, encodeJson)
import Data.Array (filter)
import Data.Either (Either(..))
import Data.Foldable (sequence_)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Generic.Rep (class Generic)
import Data.HTTP.Method (CustomMethod, Method)
import Data.Maybe (Maybe(..))
import Data.Set as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as HK
import Prelude (Unit, bind, pure, ($), (<#>), (<$>), (<<<), (<>), (>>>))
import Web.Event.Event (Event, EventType(..))

type Tradukil
  = forall x. Coercible x Tradukenda => x -> String

type HHTML m uz
  = forall w. HK.Hook m uz (HH.HTML w (HK.HookM m Unit))

newtype Tradukenda
  = Tradukenda String

fperm :: S.Set Char -> String -> Maybe String
fperm p = fapl (toCharArray >>> filter (_ `S.member` p) >>> fromCharArray)

fdevas :: ∀ a. (a -> Boolean) -> a -> Maybe a
fdevas f x =
  if f x then
    Just x
  else
    Nothing

fapl :: ∀ a b. (a -> b) -> a -> Maybe b
fapl f = Just <<< f

foreign import traktiInput :: Fn2 (String -> Maybe String) Event (Effect (Maybe String))

fen ::
  ∀ m r.
  MonadEffect m =>
  (String -> Maybe String) ->
  (String -> HK.HookM m Unit) ->
  HH.IProp ( onInput :: Event, value :: String | r ) (HK.HookM m Unit)
fen pip gxis =
  HE.handler (EventType "input") \e -> do
    gxiso <- liftEffect $ runFn2 traktiInput pip e
    sequence_ $ gxis <$> gxiso

setigi :: String -> S.Set Char
setigi = S.fromFoldable <<< toCharArray

striktAlfabet :: S.Set Char
striktAlfabet = setigi "abcdefghijklmnopqrstuvwxyz0123456789.-_"

mapErar :: ∀ l r d. (l -> r) -> Either l d -> Either r d
mapErar f (Left x) = Left $ f x

mapErar _ (Right x) = Right x

type ApiMethod
  = Either Method CustomMethod

type ApiMsg
  = { tag :: String, contents :: Maybe Json }

petKern ::
  ∀ v m r.
  EncodeJson v =>
  MonadAff m =>
  DecodeJson r =>
  ApiMethod ->
  String ->
  v ->
  m (Either Erar r)
petKern method url v = do
  kResp <-
    liftAff
      $ Affj.request
          ( Affj.defaultRequest
              { url = "/kern/" <> url
              , method = method
              , content = Just (Affj.Pet.json $ encodeJson v)
              , responseFormat = Affj.Resp.json
              }
          )
  pure do
    jsonResp <- mapErar (sE <<< RetErar) $ kResp <#> \x -> x.body
    resp :: ApiMsg <-
      mapErar (sE <<< JsonErar) $ decodeJson jsonResp
    case resp of
      { tag: "Sukc", contents: Just dat } -> mapErar (sE <<< JsonErar) $ decodeJson dat
      { tag: "ServilErar" } -> Left $ sE ServilEnaErar
      { tag: "KlientErar", contents: Just dat } -> case malkodiKlientErar dat of
        Left e -> Left $ ServilErar e
        Right e -> Left $ KlientErar e
      { tag, contents } -> Left $ sE $ NekonataVarErar tag contents
  where
  sE = ServilErar

data Erar
  = KlientErar KlientErar
  | ServilErar ServilErar

data KlientErar
  = MalgxustaRetposxtErar
  | DomajnoNeEkzistasErar

data ServilErar
  = ServilEnaErar
  | RetErar Affj.Error
  | JsonErar JsonDecodeError -- Fiaskis elanaizi servilan respondon.
  | NekonataVarErar String (Maybe Json)

derive instance genKE :: Generic KlientErar _

malkodiKlientErar :: Json -> Either ServilErar KlientErar
malkodiKlientErar erar = do
  val :: ApiMsg <- mapErar JsonErar $ decodeJson erar
  case val of
    { tag: "MalgxustaRetposxtErar" } -> Right $ MalgxustaRetposxtErar
    { tag: "DomajnoNeEkzistasErar" } -> Right $ DomajnoNeEkzistasErar
    { tag, contents } -> Left $ NekonataVarErar tag contents

-- | Priskribi klientan eraron
skrKErar :: Tradukil -> KlientErar -> String
skrKErar trd = case _ of
  MalgxustaRetposxtErar -> trd "erar.klient.malgxusta-retposxt"
  DomajnoNeEkzistasErar -> trd "erar.klient.domajno-ne-ekzistas"
