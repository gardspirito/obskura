module NunaAgo exposing (..)

import Array exposing (fromList, get)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Http
import I18Next exposing (Translations)
import Json.Decode as D
import Lingvar as L
import Maybe exposing (andThen)
import Mesagxoj exposing (Msg(..), NAAuxMsg(..), NunaAgoMsg(..))
import Set exposing (member)
import String exposing (all, filter, length, split, toLower)


nePropaguKlak : Attribute Msg
nePropaguKlak =
    stopPropagationOn "click" (D.succeed ( Nul, True ))


type Model
    = AuxMod { adr : String, erar : String, respAtend : Bool }


cxuRompebla : Model -> Bool
cxuRompebla (AuxMod { respAtend }) =
    not respAtend


gxis : NunaAgoMsg -> Model -> ( Model, Cmd Msg )
gxis msg mod =
    case ( msg, mod ) of
        ( AuxMsg auxMsg, AuxMod auxMod ) ->
            case auxMsg of
                AuxAdr adr ->
                    ( AuxMod { auxMod | adr = modifAdr auxMod.adr adr }, Cmd.none )

                AuxEnsalutu ->
                    ( AuxMod { auxMod | respAtend = True }, ensalutOrdon auxMod.adr )


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


ensalutOrdon adr =
    Http.post
        { url = "/api/ensalutu"
        , body =
            Http.multipartBody
                [ Http.stringPart "retposxt" adr
                ]
        , expect =
            Http.expectStringResponse
                (\r ->
                    Nul
                )
                (\resp ->
                    case resp of
                        Http.GoodStatus_ _ _ ->
                            Ok ()

                        Http.BadStatus_ _ erar ->
                            Err erar

                        Http.NetworkError_ ->
                            Debug.todo ""

                        _ ->
                            Debug.todo ""
                )
        }


cxuSxercist : String -> Translations -> Bool
cxuSxercist adr l =
    L.auxJamvidis l == adr


montrUzantMenu : { a | nunaAgo : Maybe Model, l : Translations } -> List (Html Mesagxoj.Msg)
montrUzantMenu m =
    case m.nunaAgo of
        Nothing ->
            []

        Just (AuxMod { adr, erar, respAtend }) ->
            let
                sxerc =
                    adr == L.auxRetposxt m.l

                erarTekst =
                    if sxerc then
                        L.auxJamvidis m.l

                    else
                        erar
            in
            [ div [ id "uzant-menu", nePropaguKlak ]
                ([ text <| L.auxAuxtentigxo m.l
                 , input
                    [ type_ "text"
                    , placeholder <| L.auxRetposxt m.l
                    , disabled respAtend
                    , value adr
                    , onInput (Mesagxoj.NunaAgoMsg << AuxMsg << AuxAdr)
                    ]
                    []
                 , button
                    [ disabled <| sxerc || (not <| cxuVeraAdr adr)
                    , onClick (Mesagxoj.NunaAgoMsg <| AuxMsg AuxEnsalutu)
                    ]
                    [ text <| L.auxEnsalutu m.l ]
                 ]
                    ++ (if erarTekst /= "" then
                            [ div [ class "erar" ] [ text erarTekst ] ]

                        else
                            []
                       )
                )
            ]
