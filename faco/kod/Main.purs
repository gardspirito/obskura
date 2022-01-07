module Main
  ( erarList
  , main
  , petLingv
  ) where

import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.Map as HM
import Data.Map as M
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple.Nested ((/\), type (/\))
import Datum (Erar, Lingvo, mapErar, petKern)
import Effect (Effect)
import Effect.Aff (Milliseconds(..), delay)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Class.Console (errorShow)
import Effect.Ref as Ref
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (type (<>))
import Halogen.Hooks as HK
import Halogen.VDom.Driver (runUI)
import Prelude (class Show, Unit, bind, discard, map, pure, unit, ($), (+), (<#>), (>>=), (>=), (>>>))
import UzantMenu as UzMenu

disvolvi :: forall m a. MonadEffect m => Show a => a -> Either String a -> m a
disvolvi a (Left erar) = do
  errorShow erar
  pure a

disvolvi _ (Right r) = pure r

petLingv :: forall m. MonadAff m => m Lingvo
petLingv =
  petKern (Left GET) "lingvar" unit
    >>= (mapErar (\_ -> "Gardanto estas fiulo") >>> disvolvi HM.empty)
    <#> (\m p -> fromMaybe "[...]" $ HM.lookup p m)

-- ERAR! INFORMU UZANTON PRI LA ERARO!
-- ... mi elradikigu ĉi tiun abominaĵon.
main :: Effect Unit
main =
  HA.runHalogenAff do
    lin <- petLingv
    pagx <- HA.awaitBody
    runUI (komp lin) unit pagx

komp :: ∀ p en el m. MonadAff m => Lingvo -> H.Component p en el m
komp lin =
  HK.component \_ _ -> HK.do
    menuH <- UzMenu.komp lin
    HK.pure
      $ HH.div
          [ HP.id "supr"
          ]
          [ HH.div
              [ HP.id "uzant"
              ]
              [ menuH
              ]
          ]

type UzErarList
  = HK.UseState (M.Map Int ErarElem) <> HK.UseState Int <> HK.Pure

type ErarElem
  = { elem :: Erar, morto :: Ref.Ref (Maybe Int) }

erarList :: ∀ m w. MonadAff m => HK.Hook m UzErarList ((Erar -> HK.HookM m Unit) /\ (HH.HTML w (HK.HookM m Unit)))
erarList = HK.do
  _ /\ statId <- HK.useState M.empty
  _ /\ nombril <- HK.useState 0
  HK.pure
    $ ( \erar -> do
          nombro <- HK.modify nombril (_ + 1)
          ref <- liftEffect $ Ref.new (Just 0)
          HK.modify_ statId (M.insert nombro { elem: erar, morto: ref })
          liftAff $ atendMorton ref
          HK.modify_ statId (M.delete nombro)
      )
    /\ HH.div
        [ HP.id "erar-list" ]
        []
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
