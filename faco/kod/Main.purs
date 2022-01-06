module Main
  ( main
  , petLingv
  ) where

import Affjax (get, printError)
import Affjax.ResponseFormat (json)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Data.Map as HM
import Data.Tuple (Tuple(..))
import Datum (Lingvo)
import Effect (Effect)
import Effect.Aff.Class (class MonadAff, liftAff)
import Effect.Class (class MonadEffect)
import Effect.Class.Console (errorShow)
import Foreign.Object (toArrayWithKey)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks as HK
import Halogen.VDom.Driver (runUI)
import Prelude (class Show, Unit, bind, discard, pure, show, unit, ($), (<#>), (>>=))
import UzantMenu as UzMenu

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
    lin <- petLingv
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

{-
montr :: forall ag m. Unit -> H.ComponentHTML ag Fakoj m
montr _ =
  HH.div
    [ HP.id "supr"
    ]
    [ HH.div
        [ HP.id "uzant"
        ]
        [ HH.slot_ UzMenu.proxy 0 UzMenu.comp unit
        ]
    ]
-}
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
