module Main
  ( main
  ) where

import Prelude
import Effect (Effect)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import UzantMenu as UzMenu

main :: Effect Unit
main =
  HA.runHalogenAff do
    pagx <- HA.awaitBody
    runUI komp unit pagx

type Fakoj
  = ( uzantMenu :: forall p. H.Slot p Void Int )

komp :: forall p en el m. H.Component p en el m
komp =
  H.mkComponent
    { initialState: const unit
    , render: montr
    , eval: H.mkEval H.defaultEval
    }

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
