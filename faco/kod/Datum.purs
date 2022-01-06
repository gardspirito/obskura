module Datum
  ( HHTML
  , Lingvo
  , fapl
  , fdevas
  , fen
  , fperm
  , setigi
  , striktAlfabet
  , traduk
  ) where

import Data.Array (filter)
import Data.Foldable (sequence_)
import Data.Function.Uncurried (Fn2, runFn2)
import Data.Map (Map, lookup)
import Data.Maybe (fromMaybe, Maybe(..))
import Data.Set as S
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.Hooks as HK
import Prelude (Unit, ($), (<<<), (>>>), bind, (<$>))
import Web.Event.Event (Event, EventType(..))

type Lingvo
  = Map String String

type HHTML m uz
  = forall w. HK.Hook m uz (HH.HTML w (HK.HookM m Unit))

traduk :: Lingvo -> String -> String
traduk l p = fromMaybe "[...]" $ lookup p l

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
