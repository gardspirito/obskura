module UzantMenu
  ( komp
  , Stat
  ) where

import DOM.HTML.Indexed.InputType (InputType(InputText))
import Data.Array (filter, length, all)
import Data.Either (Either(..))
import Data.Foldable (elem)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Data.String.CodeUnits as S
import Data.String.Common (split, toLower)
import Data.Tuple.Nested ((/\))
import Datum (Erar(..), HHTML, KlientErar(..), Tradukil, Eraril, fapl, fdevas, fen, fperm, petKern, setigi, skrKErar, striktAlfabet)
import Effect.Aff.Class (class MonadAff)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Hooks (type (<>))
import Halogen.Hooks as HK
import Prelude (bind, const, when, ($), (&&), (<), (<>), (==), (>=), (>=>), (>>>))
import Web.UIEvent.KeyboardEvent as KeyboardEvent

data Stat
  = Aux { retposxt :: String, erar :: Maybe (Tradukil -> String) }
  | Sukc

type UzMenu
  = HK.UseState Stat <> HK.Pure

komp ∷ ∀ m. MonadAff m => Eraril -> Tradukil -> HHTML m UzMenu
komp eraril trd = HK.do
  stat /\ statId <- HK.useState $ Aux { retposxt: "", erar: Nothing }
  HK.pure
    $ HH.div
        [ HP.id "uzant-menu"
        ] case stat of
        Aux orstat@{ retposxt } ->
          let
            veraAdr = verigiAdr retposxt

            sendi = do
              resp <- petKern (Left POST) "ensaluti" { retposxt: retposxt }
              case resp of
                Left (KlientErar erar)
                  | erar `elem` [ MalgxustaRetposxtErar, DomajnoNeEkzistasErar ] -> HK.put statId $ Aux $ orstat { erar = Just $ skrKErar erar }
                Left n -> eraril n
                Right (_ :: {}) -> HK.put statId $ Sukc
          in
            [ HH.text $ trd "aux.auxtentigxo"
            , HH.input
                [ HP.type_ InputText
                , HP.placeholder (trd "aux.retposxt")
                , fen
                    (fdevas (kalkDe '@' >>> (_ < 2)) >=> fapl toLower >=> fperm (striktAlfabet <> setigi "@"))
                    (\adr -> HK.put statId $ Aux $ orstat { retposxt = adr })
                , HE.onKeyDown
                    $ \event -> when (veraAdr && KeyboardEvent.code event == "Enter") sendi
                ]
            , HH.button
                [ HP.enabled $ veraAdr
                , HE.onClick $ const sendi
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
