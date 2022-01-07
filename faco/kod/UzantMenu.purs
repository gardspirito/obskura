module UzantMenu
  ( komp
  , Stat
  ) where

import DOM.HTML.Indexed.InputType (InputType(InputText))
import Data.Array (filter, length, all)
import Data.String (Pattern(..))
import Data.String.CodeUnits as S
import Data.String.Common (split, toLower)
import Data.Tuple.Nested ((/\))
import Datum (HHTML, Lingvo, fapl, fdevas, fen, fperm, setigi, striktAlfabet)
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.Hooks (type (<>))
import Halogen.Hooks as HK
import Prelude (($), (>=>), (==), (<), (>>>), (<>), (>=), (&&))

data Stat
  = Aux { retposxt :: String }
  | Sukc

type UzMenu
  = HK.UseState Stat <> HK.Pure

komp ∷ ∀ m. MonadAff m => Lingvo -> HHTML m UzMenu
komp trd = HK.do
  stat /\ statId <- HK.useState $ Aux { retposxt: "" }
  HK.pure
    $ HH.div
        [ HP.id "uzant-menu"
        ] case stat of
        Aux orstat@{ retposxt } ->
          [ HH.text $ trd "aux.auxtentigxo"
          , HH.input
              [ HP.type_ InputText
              , HP.placeholder (trd "aux.retposxt")
              , fen
                  (fdevas (kalkDe '@' >>> (_ < 2)) >=> fapl toLower >=> fperm (striktAlfabet <> setigi "@"))
                  (\adr -> HK.put statId $ Aux $ orstat { retposxt = adr })
              ]
          , HH.button
              [ HP.enabled $ verigiAdr retposxt
              --, HP.onClick 
              ]
              [ HH.text $ trd "aux.ensalutu" ]
          ]
        Sukc -> []
  where
  kalkDe liter = S.toCharArray >>> filter (_ == liter) >>> length

  verigiAdr adr
    | [ un, du ] <- split (Pattern "@") adr = S.length un >= 1 && kalkDe '.' du >= 1 && verigiDu du
      where
      verigiDu = split (Pattern ".") >>> all (\domajnpart -> S.length domajnpart >= 2)

  verigiAdr _ = false

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
