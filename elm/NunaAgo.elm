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
import Set exposing (member)
import String exposing (all, filter, length, split, toLower)


nePropaguKlak : (Msg -> msg) -> Attribute msg
nePropaguKlak f =
    stopPropagationOn "click" (D.succeed ( f Nul, True ))


type Model
    = AuxMod { adr : String, erar : String, respAtend : Bool }


type Msg
    = AuxMsg AuxMsg
    | Nul


type AuxMsg
    = Adr String
    | Ensalutu


cxuRompebla : Model -> Bool
cxuRompebla (AuxMod { respAtend }) =
    not respAtend


gxis : (Msg -> msg) -> Msg -> Model -> ( Model, Cmd msg )
gxis kunt msg mod =
    case ( msg, mod ) of
        ( AuxMsg auxMsg, AuxMod auxMod ) ->
            case auxMsg of
                Adr adr ->
                    ( AuxMod { auxMod | adr = modifAdr auxMod.adr adr }, Cmd.none )

                Ensalutu ->
                    ( AuxMod { auxMod | respAtend = True }, ensalutOrdon kunt auxMod.adr )

        ( Nul, _ ) ->
            ( mod, Cmd.none )


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


ensalutOrdon kunt adr =
    Http.post
        { url = "/api/ensalutu"
        , body =
            Http.multipartBody
                [ Http.stringPart "retposxt" adr
                ]
        , expect =
            Http.expectStringResponse
                (\r ->
                    kunt Nul
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


montrUzantMenu : { a | nunaAgo : Maybe Model, l : Translations } -> (Msg -> msg) -> List (Html msg)
montrUzantMenu m kunt =
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
            [ div [ id "uzant-menu", nePropaguKlak kunt ]
                ([ text <| L.auxAuxtentigxo m.l
                 , input
                    [ type_ "text"
                    , placeholder <| L.auxRetposxt m.l
                    , disabled respAtend
                    , value adr
                    , onInput (kunt << AuxMsg << Adr)
                    ]
                    []
                 , button
                    [ disabled <| sxerc || (not <| cxuVeraAdr adr)
                    , onClick (kunt <| AuxMsg Ensalutu)
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
