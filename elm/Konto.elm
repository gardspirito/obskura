module Konto exposing (..)

import Char exposing (toLower)
import Cxies exposing (KontKreMsg(..), Msg(..), PagxMsg(..), alfabeto, bildFormatoj, ciferKajSimDis, simDis)
import Dict exposing (Dict)
import File exposing (File, mime)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput, preventDefaultOn)
import I18Next exposing (Translations)
import Json.Decode as Decode exposing (field, index)
import Lingvar as L
import Platform.Cmd exposing (Cmd)
import Set exposing (Set)
import String exposing (length)
import Svg exposing (path, svg)
import Svg.Attributes exposing (d, viewBox)
import Task exposing (perform)
import Tuple exposing (first)


type alias Model =
    { nomo : String, id : String, uzantbild : Maybe File, uzantbildUrl : Maybe String, pri : String }


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
            , id = kreID nom
        }


kreID : String -> String
kreID x =
    x
        |> String.toList
        |> List.concatMap latinigi
        |> (first << forDisRipet True)
        |> String.fromList


latinigi : Char -> List Char
latinigi x =
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


latinigVortar : Dict Char String
latinigVortar =
    Dict.fromList
        [ ( 'а', "a" )
        , ( 'б', "b" )
        , ( 'в', "v" )
        , ( 'г', "g" )
        , ( 'д', "d" )
        , ( 'е', "e" )
        , ( 'ё', "je" )
        , ( 'ж', "zh" )
        , ( 'з', "z" )
        , ( 'и', "i" )
        , ( 'й', "ji" )
        , ( 'к', "k" )
        , ( 'л', "l" )
        , ( 'м', "m" )
        , ( 'н', "n" )
        , ( 'о', "o" )
        , ( 'п', "p" )
        , ( 'р', "r" )
        , ( 'с', "s" )
        , ( 'т', "t" )
        , ( 'у', "u" )
        , ( 'ф', "f" )
        , ( 'х', "h" )
        , ( 'ц', "c" )
        , ( 'ч', "ch" )
        , ( 'ш', "sh" )
        , ( 'щ', "sh" )
        , ( 'ы', "i" )
        , ( 'э', "e" )
        , ( 'ю', "ju" )
        , ( 'я', "ja" )
        ]


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
            ]
        ]
    ]


signAlsxult : Html a
signAlsxult =
    svg [ viewBox "0 0 16 16", id "sign-alsxult" ]
        [ path [ d "M.5 9.9a.5.5 0 0 1 .5.5v2.5a1 1 0 0 0 1 1h12a1 1 0 0 0 1-1v-2.5a.5.5 0 0 1 1 0v2.5a2 2 0 0 1-2 2H2a2 2 0 0 1-2-2v-2.5a.5.5 0 0 1 .5-.5z" ] []
        , path [ d "M7.646 1.146a.5.5 0 0 1 .708 0l3 3a.5.5 0 0 1-.708.708L8.5 2.707V11.5a.5.5 0 0 1-1 0V2.707L5.354 4.854a.5.5 0 1 1-.708-.708l3-3z" ] []
        ]
