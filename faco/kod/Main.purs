module Main
  ( erarList
  , komp
  , main
  )
  where

import Affjax as Affj
import Data.Argonaut.Core (stringify)
import Data.Argonaut.Decode (printJsonDecodeError)
import Data.Array as Arr
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map as HM
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Show.Generic (genericShow)
import Data.Tuple (Tuple(..))
import Data.Tuple.Nested ((/\), type (/\))
import Datum (Erar(..), ServilErar(..), Tradukenda(..), Tradukil, petKern, skrKErar)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (liftEffect)
import Effect.Console (error)
import Effect.Ref as Ref
import Foreign.Object (toArrayWithKey)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (type (<>), fork, useLifecycleEffect)
import Halogen.Hooks as HK
import Halogen.VDom.Driver (runUI)
import Prelude (Unit, bind, discard, map, pure, show, unit, ($), (+), (<#>), (<<<), (<>), (>=), (<$>))
import Safe.Coerce (coerce)
import Stil as KL
import UzantMenu as UzMenu

main :: Effect Unit
main =
  HA.runHalogenAff do
    pagx <- HA.awaitBody
    runUI komp unit pagx

komp :: ∀ p en el m. MonadAff m => H.Component p en el m
komp =
  HK.component \_ _ -> HK.do
    linMap /\ linId <- HK.useState HM.empty
    let
      trd = (\p -> fromMaybe "[...]" (HM.lookup (coerce p) linMap)) :: Tradukil
    erarilo /\ erarH <- erarList trd
    useLifecycleEffect do
      respond <- petKern (Left GET) "lingvar" unit
      case respond of
        Left erar -> erarilo erar
        Right nlin -> HK.put linId $ HM.fromFoldable (toArrayWithKey Tuple nlin)
      pure Nothing
    menuH <- UzMenu.komp trd
    HK.pure
      $ HH.div
          [ HP.id "supr"
          ]
          [ HH.div
              [ HP.id "uzant"
              ]
              [ menuH
              ]
          , erarH
          ]

type UzErarList
  = HK.UseState (M.Map Int ErarElem) <> HK.UseState Int <> HK.Pure

type ErarElem
  = { elem :: Erar, morto :: Ref.Ref (Maybe Int) }

erarList :: ∀ m w. MonadAff m => Tradukil -> HK.Hook m UzErarList ((Erar -> HK.HookM m Unit) /\ (HH.HTML w (HK.HookM m Unit)))
erarList trd = HK.do
  stat /\ statId <- HK.useState M.empty
  _ /\ nombril <- HK.useState 0
  HK.pure
    $ ( \erar -> do
          liftEffect $ raportiErar erar
          nombro <- HK.modify nombril (_ + 1)
          ref <- liftEffect $ Ref.new (Just 0)
          HK.modify_ statId (M.insert nombro { elem: erar, morto: ref })
          _ <-
            fork do
              liftAff $ atendMorton ref
              HK.modify_ statId (M.delete nombro)
          pure unit
      )
    /\ HH.div
        [ HP.id "erar-list" ]
        ( Arr.fromFoldable $ M.values stat
            <#> \erarEl ->
                let
                  titol /\ info = montrErar erarEl.elem
                in
                  HH.div (revivigiMsg erarEl.morto)
                    $ [ HH.h1_ [ HH.text titol ]
                      , HH.hr_
                      ]
                    <> info
        )
  where
  atendMorton ref = cikl
    where
    morttempo = 5

    cikl = do
      delay (Milliseconds 1000.0)
      val <- liftEffect $ Ref.modify (map (_ + 1)) ref
      if fromMaybe 0 val >= morttempo then
        pure unit
      else
        cikl

  revivigiMsg ref =
    [ HE.onMouseEnter \_ -> liftEffect $ Ref.write Nothing ref
    , HE.onMouseLeave \_ -> liftEffect $ Ref.write (Just 0) ref
    ]

  montrErar :: ∀ i. Erar -> String /\ Array (HH.HTML w i)
  montrErar = case _ of
    ServilErar serar ->
      trd "erar.servil"
        /\ ( case serar of
              ServilEnaErar -> [ htrd "erar.servil.ena", HH.em [ HP.class_ KL.etaTekst ] $ [ htrd "erar.servil.ena.sub" ] ] -- Farende: Malgranda
              RetErar erar -> [ htrd "erar.servil.ret", HH.br_, HH.text $ Affj.printError erar ]
              JsonErar erar -> [ htrd "erar.servil.malkod", HH.br_, HH.text $ printJsonDecodeError erar ]
              NekonataVarErar tag contents -> [ htrd "erar.servil.nekonata-var", HH.br_, HH.text tag, HH.text $ show $ stringify <$> contents ]
          )
    KlientErar erar -> trd "erar.klient" /\ [ HH.text $ skrKErar trd erar ]
    where
    htrd = HH.text <<< trd

  raportiErar :: Erar -> Effect Unit
  raportiErar origErar =
    error
      $ case origErar of
          ServilErar serar -> case serar of
            ServilEnaErar -> "Server reported internal error"
            RetErar erar -> "Error while attemping to perform Affjax request:\n" <> Affj.printError erar
            JsonErar erar -> "Failed to parse server response as JSON:\n" <> printJsonDecodeError erar
            NekonataVarErar tag contents -> "Server responded with unknown response \"" <> tag <> "\":\n" <> (fromMaybe "[... no body provided ...]" $ stringify <$> contents)
          KlientErar x -> genericShow x
