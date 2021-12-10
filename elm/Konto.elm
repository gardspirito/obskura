module Konto exposing (..)

import Char exposing (toLower)
import Cxies exposing (KontKreMsg(..), Msg(..), PagxMsg(..), alfabeto, atendJson, bildFormatoj, ciferKajSimDis, raportiErar, simDis)
import Dict exposing (Dict)
import File exposing (File, mime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import Http
import I18Next exposing (Translations)
import Json.Decode as Decode exposing (field, index)
import Lingvar as L
import Platform.Cmd exposing (Cmd)
import Set exposing (Set)
import String exposing (length, uncons)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, viewBox)
import Task exposing (perform)
import Tuple exposing (first, mapFirst, mapSecond)


type alias Model =
    { nomo : String
    , id : String
    , uzantbild : Maybe File
    , uzantbildUrl : Maybe String
    , pri : String
    , latinigVortar : Dict Char String
    , erar : String
    }


komenci : ( Model, Cmd Msg )
komenci =
    ( { nomo = ""
      , id = ""
      , uzantbild = Nothing
      , uzantbildUrl = Nothing
      , pri = ""
      , latinigVortar = Dict.fromList []
      , erar = ""
      }
    , Http.get
        { url = "/uzantid-anst.json"
        , expect =
            Http.expectStringResponse
                (PagxMsg << KontKreMsg << LatinigVort)
            <|
                atendJson (Decode.keyValuePairs Decode.string)
        }
      -- ripari
    )


gxis : KontKreMsg -> Model -> ( Model, Cmd Msg )
gxis msg mod =
    case msg of
        UzantNom nom ->
            ( gxisNomon mod nom, Cmd.none )

        UzantBild dos ->
            ( { mod | uzantbild = Just dos }
            , perform (PagxMsg << KontKreMsg << UzantBildUrl) <| File.toUrl dos
            )

        UzantBildUrl url ->
            ( { mod | uzantbildUrl = Just url }, Cmd.none )

        UzantPri pri ->
            ( { mod | pri = pri }, Cmd.none )

        LatinigVort rez ->
            case rez of
                Err x ->
                    ( { mod | erar = x }, Cmd.none )

                Ok x ->
                    mapFirst (\vortar -> { mod | latinigVortar = vortar }) <| listAlVort x


listAlVort : List ( String, String ) -> ( Dict Char String, Cmd msg )
listAlVort list =
    case list of
        ( nomo, val ) :: xs ->
            case uncons nomo of
                Just ( l, "" ) ->
                    mapFirst (Dict.insert l val) <| listAlVort xs

                _ ->
                    let
                        erar =
                            "You instructed to replace \"" ++ nomo ++ "\" in user identifiers with \"" ++ val ++ "\". Unfortunately, I'm not capable of multi-character replacements. If you think that you really need this to be possible, open a new issue: https://github.com/gardspirito/obskurativ/issues/new. Thank you!"
                    in
                    mapSecond
                        (\x ->
                            Cmd.batch
                                [ x
                                , raportiErar erar
                                ]
                        )
                    <|
                        listAlVort xs

        [] ->
            ( Dict.empty, Cmd.none )


nomLim : Int
nomLim =
    45


gxisNomon : Model -> String -> Model
gxisNomon m nom_ =
    let
        nom =
            String.replace "  " " " nom_
    in
    if length nom > nomLim then
        m

    else
        { m
            | nomo = nom
            , id = kreID nom m.latinigVortar
        }


kreID : String -> Dict Char String -> String
kreID x latinigVortar =
    x
        |> String.toList
        |> List.concatMap (latinigi latinigVortar)
        |> (first << forDisRipet True)
        |> String.fromList


latinigi : Dict Char String -> Char -> List Char
latinigi latinigVortar x =
    let
        l =
            toLower x
    in
    if Set.member l idPerm then
        [ l ]

    else if l == ' ' then
        [ '-' ]

    else
        case Dict.get l latinigVortar of
            Just lat ->
                String.toList lat

            Nothing ->
                []


idPerm : Set Char
idPerm =
    Set.fromList <| alfabeto ++ ciferKajSimDis


forDisRipet : Bool -> List Char -> ( List Char, Bool )
forDisRipet disMalant list =
    case list of
        [] ->
            ( [], True )

        x :: xs ->
            let
                disNun =
                    Set.member x dis

                ( rez, finAnt ) =
                    forDisRipet disNun xs
            in
            if disNun && (finAnt || disMalant) then
                ( rez, finAnt )

            else
                ( x :: rez, False )


dis : Set Char
dis =
    Set.fromList simDis


onFileDrop : Attribute Msg
onFileDrop =
    preventDefaultOn "drop" <|
        Decode.map
            (\dos ->
                ( if List.member (mime dos) bildFormatoj then
                    PagxMsg <| KontKreMsg <| UzantBild dos

                  else
                    NulMsg
                , True
                )
            )
        <|
            field "dataTransfer" <|
                field "files" <|
                    index 0 <|
                        File.decoder


onDragover : Attribute Msg
onDragover =
    preventDefaultOn "dragover" <| Decode.succeed ( NulMsg, True )


montr : Model -> Translations -> List (Html Msg)
montr mod l =
    [ div
        [ id "krei-kont", class "meze pagx-largxa" ]
        [ div [ class "kolumn" ]
            [ div [ id "krei-kont-bonvenon" ] [ text <| L.auxKreiKontonBonvenon l ]
            , hr [] []
            , div
                ([ onClick <| BildInstal <| PagxMsg << KontKreMsg << UzantBild
                 , onFileDrop
                 , onDragover
                 , class "meze"
                 ]
                    ++ (case mod.uzantbildUrl of
                            Just x ->
                                [ id "krei-uzant-bild-alsxultit"
                                , style "background-image" ("url(" ++ x ++ ")")
                                ]

                            Nothing ->
                                [ id "krei-uzant-bild"
                                ]
                       )
                )
                (case mod.uzantbildUrl of
                    Nothing ->
                        [ signAlsxult, div [ id "krei-uzant-bild-konsileto" ] [ text <| L.auxKreiKontonAlsxult l ] ]

                    _ ->
                        []
                )
            , input
                [ type_ "text"
                , onInput (PagxMsg << KontKreMsg << UzantNom)
                , value mod.nomo
                , placeholder <| L.auxKreiKontonUzantnomo l
                ]
                []
            , span
                [ class "eta-teksto"
                , style "display"
                    (if mod.id /= "" then
                        "block"

                     else
                        "none"
                    )
                ]
                [ text <| "(" ++ mod.id ++ ")" ]
            , textarea
                [ onInput (PagxMsg << KontKreMsg << UzantPri)
                , placeholder <|
                    L.auxKreiKontonUzantpri l
                , maxlength 500
                ]
                []
            , button [ disabled <| not <| (length mod.id >= 3) ] [ text <| L.auxKreiKontonFini l ]
            , div
                [ class "erar"
                , style "display"
                    (if mod.erar /= "" then
                        "block"

                     else
                        "none"
                    )
                ]
                [ text mod.erar ]
            ]
        ]
    ]


signAlsxult : Html a
signAlsxult =
    svg [ viewBox "0 0 16 16", id "sign-alsxult" ]
        [ path [ d "M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5z" ] []
        , path [ d "M7.646 1.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1-.708.708L8.5 2.707V11.5a.5.5 0 0 1-1 0V2.707L5.354 4.854a.5.5 0 1 1-.708-.708l3-3z" ] []
        ]
