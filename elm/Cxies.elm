port module Cxies exposing (..)

import Browser exposing (UrlRequest(..))
import File exposing (File)
import Http exposing (Expect)
import I18Next exposing (Translations)
import Json.Decode exposing (Decoder, decodeString, errorToString)
import Lingvar as L
import Result exposing (mapError)


infix left  7 (/) = fdiv


port akirejo : (( String, String ) -> msg) -> Sub msg


port konservu : ( String, String ) -> Cmd msg


port akiru : String -> Cmd msg


port raportiErar : String -> Cmd msg



-- Pasu konvertilo de respondteksto al Rezulto por akiri funkcion uzeblan kiel analizilo en expectStringResponse.
-- Ĉi tiu metodo aŭtomate traktas ret erarojn kaj eraroj de malbona statuso.


type PetErar
    = KonektErar
    | AliaErar String


atend : (Result PetErar String -> x) -> Expect x
atend sendil =
    Http.expectStringResponse sendil
        (\resp ->
            case resp of
                Http.GoodStatus_ _ tekst ->
                    Ok tekst

                Http.BadStatus_ _ erar ->
                    Err <| AliaErar erar

                Http.NetworkError_ ->
                    Err <| KonektErar

                _ ->
                    Debug.todo ""
        )


montrErar : PetErar -> Translations -> String
montrErar er l =
    (case er of
        KonektErar ->
            L.erarKonekt

        AliaErar "SERVILO_NE_EKZISTAS" ->
            L.auxErarNeEkzistas

        AliaErar t ->
            always t
    )
        l



-- Turn Just x to [x] and Nothing to []


mayList : Maybe x -> List x
mayList m =
    case m of
        Just x ->
            [ x ]

        Nothing ->
            []


atendJson : Decoder a -> (Result PetErar a -> x) -> Expect x
atendJson malkodil sendil =
    atend <| Result.andThen (decodeString malkodil >> mapError (AliaErar << errorToString)) >> sendil


bildFormatoj : List String
bildFormatoj =
    [ "image/png", "image/jpeg", "image/webp" ]


alfabeto : List Char
alfabeto =
    String.toList <| "abcdefghijklmnopqrstuvwxyz"


ciferKajSimDis : List Char
ciferKajSimDis =
    (String.toList <| "0123456789") ++ simDis


simDis : List Char
simDis =
    String.toList <| ".-_"


type Msg
    = NunaAgoMsg NunaAgoMsg
    | PagxMsg PagxMsg
    | Malferm Malferm
    | Akir ( String, String )
    | UrlSxan UrlRequest
    | BildInstal (File -> Msg)
    | NulMsg


type Malferm
    = Ferm
    | MalfermUzMenu


type NunaAgoMsg
    = AuxMsg NAAuxMsg


type NAAuxMsg
    = AuxAdr String
    | AuxEnsalutu
    | AuxEnsalutRes (Result PetErar ())


type PagxMsg
    = KontKreMsg KontKreMsg


type KontKreMsg
    = UzantNom String
    | UzantBild File
    | UzantBildUrl String
    | UzantPri String
    | KreiSend
    | LatinigVort (Result PetErar (List ( String, String )))
