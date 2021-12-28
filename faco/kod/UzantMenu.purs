module UzantMenu
  ( komp, proxy
  ) where

import Prelude
import Type.Proxy (Proxy(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Datum

proxy = Proxy :: Proxy "uzantMenu"

data Stat
  = Aux { retposxt :: String }
  | Sukc

komp ∷ ∀ p en el m. LinKomp p en el m
komp =
  mkLinKomp
    { initialState: const (Aux { retposxt: "" })
    , render: montr
    , eval: H.mkEval H.defaultEval
    }

montr :: forall ag m. Lingvo -> Stat -> H.ComponentHTML ag () m
montr lin stat =
  HH.div
    [ HP.id "uzant-menu"
    ]
    [
        case stat of
            Aux { retposxt } -> HH.div [] [
                    htraduk "aux.ensalutu" lin
                ]
            Sukc -> HH.div [] []
    ]

{-

montrUzantMenu : { a | nunaAgo : Maybe Model, l : Translations } -> List (Html Msg)
montrUzantMenu m =
    case m.nunaAgo of
        Nothing ->
            []

        Just mod ->
            [ div [ id "uzant-menu", nePropaguKlak ] <|
                case mod of
                    AuxMod { adr, erar, respAtend } ->
                        let
                            sxerc =
                                adr == L.auxRetposxt m.l

                            erarN =
                                if sxerc then
                                    L.auxJamvidis m.l

                                else
                                    erar
                        in
                        [ text <| L.auxAuxtentigxo m.l
                        , input
                            [ type_ "text"
                            , placeholder <| L.auxRetposxt m.l
                            , disabled respAtend
                            , value adr
                            , onInput (NunaAgoMsg << AuxMsg << AuxAdr)
                            ]
                            []
                        , button
                            [ disabled <| sxerc || (not <| cxuVeraAdr adr)
                            , onClick (NunaAgoMsg <| AuxMsg AuxEnsalutu)
                            ]
                            [ text <| L.auxEnsalutu m.l ]
                        ]
                            ++ (if erarTekst /= "" then
                                    [ div [ class "erar" ] [ text erarTekst ] ]

                                else
                                    []
                               )

                    AuxSukc celloko ->
                        [ text <| L.auxSukces1 m.l
                        , br [] []
                        , text <| L.auxSukces2 m.l
                        , br [] []
                        , signSukc
                        , br [] []
                        , text <| L.auxVizitu m.l
                        , text " "
                        , a [ href ("https://" ++ celloko) ] [ text celloko ]
                        ]
            ]

-}
