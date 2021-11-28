module NunaAgo exposing (..)

import Array exposing (fromList, get)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, stopPropagationOn)
import Http
import I18Next exposing (Translations)
import Json.Decode as D
import Json.Encode
import Konservejo exposing (konservu)
import Lingvar as L
import Maybe exposing (andThen, withDefault)
import Mesagxoj exposing (Msg(..), NAAuxMsg(..), NunaAgoMsg(..))
import Set exposing (member)
import String exposing (all, filter, length, split, toLower)


nePropaguKlak : Attribute Msg
nePropaguKlak =
    stopPropagationOn "click" (D.succeed ( NulMsg, True ))


type Model
    = AuxMod { adr : String, erar : String, respAtend : Bool }
    | AuxSukc String


type alias Retposxt =
    ( String, String )


cxuRompebla : Model -> Bool
cxuRompebla mod =
    case mod of
        AuxMod { respAtend } ->
            not respAtend

        AuxSukc _ ->
            True


gxis : NunaAgoMsg -> Model -> ( Model, Cmd Msg )
gxis msg mod =
    case ( msg, mod ) of
        ( AuxMsg auxMsg, AuxMod auxMod ) ->
            case auxMsg of
                AuxAdr adr ->
                    ( AuxMod { auxMod | adr = modifAdr auxMod.adr adr }, Cmd.none )

                AuxEnsalutu ->
                    ( AuxMod { auxMod | erar = "", respAtend = True }, ensalutOrdon auxMod.adr )

                AuxEnsalutRes (Ok ()) ->
                    ( AuxSukc (auxMod.adr |> split "@" |> fromList |> get 1 |> withDefault ""), konservu ( "retposxt", auxMod.adr ) )

                AuxEnsalutRes (Err erar) ->
                    ( AuxMod { auxMod | erar = erar, respAtend = False }, Cmd.none )

        _ ->
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
        (split "@" adr |> fromList |> get 1)
            |> andThen (split "." >> fromList >> get 1)
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
                (NunaAgoMsg << AuxMsg << AuxEnsalutRes)
                (\resp ->
                    case resp of
                        Http.GoodStatus_ _ _ ->
                            Ok ()

                        Http.BadStatus_ _ erar ->
                            Err erar

                        Http.NetworkError_ ->
                            Err "KONEKT"

                        _ ->
                            Debug.todo ""
                )
        }


cxuSxercist : String -> Translations -> Bool
cxuSxercist adr l =
    L.auxJamvidis l == adr


montrErar : String -> Translations -> String
montrErar x l =
    case x of
        "SERVILO_NE_EKZISTAS" ->
            L.auxErarNeEkzistas l

        "KONEKT" ->
            L.erarKonekt l

        _ ->
            x


montrUzantMenu : { a | nunaAgo : Maybe Model, l : Translations } -> List (Html Mesagxoj.Msg)
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

                            erarTekst =
                                if sxerc then
                                    L.auxJamvidis m.l

                                else
                                    montrErar erar m.l
                        in
                        [ text <| L.auxAuxtentigxo m.l
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

                    AuxSukc _ ->
                        [ node "svg" [ id "bla", property "xmlns" <| Json.Encode.string "http://www.w3.org/2000/svg", property "innerHTML" <| Json.Encode.string """<path d="M10.97 4.97a.75.75 0 0 1 1.07 1.05l-3.99 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.267.267 0 0 1 .02-.022z"/>""" ] []
                        ]
            ]


signSukc : String
signSukc =
    """
  <path d="M10.97 4.97a.75.75 0 0 1 1.07 1.05l-3.99 4.99a.75.75 0 0 1-1.08.02L4.324 8.384a.75.75 0 1 1 1.06-1.06l2.094 2.093 3.473-4.425a.267.267 0 0 1 .02-.022z"/>
  """
