module Main
  ( main
  ) where

import Prelude (class Show, Unit, Void, bind, const, discard, pure, show, unit, ($), (<#>), (>>=))
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Effect.Class.Console (errorShow)
import Affjax (get, printError)
import Affjax.ResponseFormat (json)
import Data.Argonaut.Decode (decodeJson)
import Foreign.Object (toArrayWithKey)
import Data.HashMap as HM
import Data.Either (Either(..))
import UzantMenu as UzMenu
import Effect.Class (class MonadEffect)
import Datum (Lingvo)
import Data.Tuple (Tuple(..))
import Effect.Aff.Class (class MonadAff, liftAff)

disvolvi :: forall m a. MonadEffect m => Show a => a -> Either String a -> m a
disvolvi a (Left erar) = do
  errorShow erar
  pure a

disvolvi _ (Right r) = pure r

mapLiv :: forall l r d. (l -> r) -> Either l d -> Either r d
mapLiv f (Left x) = Left $ f x
mapLiv _ (Right d) = Right d

petLingv :: forall m. MonadAff m => m Lingvo
petLingv =
  (liftAff $ get json "/kern/lingvar")
    <#> ( \respond -> do
          korpo <- mapLiv printError respond
          obj <- mapLiv show (decodeJson korpo.body)
          pure $ HM.fromFoldable (toArrayWithKey Tuple obj)
      )
    >>= (disvolvi HM.empty)

main :: Effect Unit
main =
  HA.runHalogenAff do
    pagx <- HA.awaitBody
    lingv <- petLingv
    runUI (komp lingv) unit pagx

type Fakoj
  = ( uzantMenu :: ∀ p. H.Slot p Void Int )

komp :: ∀ p en el m. Lingvo -> H.Component p en el m
komp lin =
  H.mkComponent
    { initialState: const {lin}
    , render: montr
    , eval: H.mkEval H.defaultEval
    }

montr :: ∀ ag m. {lin :: Lingvo} -> H.ComponentHTML ag Fakoj m
montr {lin} =
  HH.div
    [ HP.id "supr"
    ]
    [ HH.div
        [ HP.id "uzant"
        ]
        [ HH.slot_ UzMenu.proxy 0 UzMenu.komp {lin, ene: unit}
        ]
    ]

{-
        PagxRegula ->
            { title = "Testpaĝo"
            , body =
                [ div (rompNunanAgon model)
                    [ div [ id "supr" ]
                        [ div
                            [ id "uzant"
                            , onClick (Malferm MalfermUzMenu)
                            ]
                            (NunaAgo.montrUzantMenu model)
                        ]
                    , div [] lorem
                    ]
                ]
            }
-}
