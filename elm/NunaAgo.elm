module NunaAgo exposing (..)

import Array exposing (fromList, get)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)
import I18Next exposing (Translations)
import Lingvar as L
import Maybe exposing (andThen)
import Set exposing (member)
import String exposing (all, filter, length, split, toLower)


type Model
    = AuxMod { adr : String, erar : String }


type Msg
    = AuxMsg AuxMsg


gxis : Msg -> Model -> ( Model, Cmd msg )
gxis msg mod =
    case ( msg, mod ) of
        ( AuxMsg (Adr adr), AuxMod auxMod ) ->
            ( AuxMod { auxMod | adr = modifAdr auxMod.adr adr }, Cmd.none )


adrPerm : Set.Set Char
adrPerm =
    Set.fromList <| String.toList "abcdefghijklmnopqrstuvwxyz0123456789@.-_"


modifAdr : String -> String -> String
modifAdr ant nun_ =
    let
        nun =
            toLower nun_
    in
    if (filter ((==) '@') nun |> length) <= 1 && all (\x -> member x adrPerm) nun then
        nun

    else
        ant


cxuVeraAdr : String -> Bool
cxuVeraAdr adr =
    case
        (get 1 <| fromList <| split "@" adr)
            |> andThen (get 1 << fromList << split ".")
    of
        Just x ->
            length x >= 2

        Nothing ->
            False


cxuSxercist : String -> Translations -> Bool
cxuSxercist adr l =
    L.auxJamvidis l == adr


type AuxMsg
    = Adr String


montrUzantMenu : { a | nunaAgo : Maybe Model, l : Translations } -> (Msg -> msg) -> List (Html msg)
montrUzantMenu m kunt =
    case m.nunaAgo of
        Nothing ->
            []

        Just (AuxMod { adr, erar }) ->
            let
                sxerc =
                    adr == L.auxRetposxt m.l

                erarTekst =
                    if sxerc then
                        L.auxJamvidis m.l

                    else
                        erar
            in
            [ div [ id "uzant-menu" ]
                ([ text <| L.auxAuxtentigxo m.l
                 , input
                    [ type_ "text"
                    , placeholder <| L.auxRetposxt m.l
                    , value adr
                    , onInput (kunt << AuxMsg << Adr)
                    ]
                    []
                 , button [ disabled <| (||) sxerc <| not <| cxuVeraAdr adr ] [ text <| L.auxEnsalutu m.l ]
                 ]
                    ++ (if erarTekst /= "" then
                            [ div [ class "erar" ] [ text erarTekst ] ]

                        else
                            []
                       )
                )
            ]



{- List.singleton <|
   div [ id "supernivelo" ]
       [ div [ id "auxtent-super" ]
           [ div [ id "auxtent" ]
               [ h1 [] [ text <| L.auxAuxtentigxo m.l ]
               , label [ for "retposxt" ] [ text <| L.auxRetposxt m.l ]
               , input [ type_ "text", id "retposxt", value adr, onInput (kunt << AuxMsg << Adr) ] []
               , button [] [ text <| L.auxEnsalutu m.l ]
               ]
           ]
       ]
-}
