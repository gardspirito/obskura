port module Cxies exposing (..)

import Browser exposing (UrlRequest(..))
import File exposing (File)
import Http exposing (Response)
import Json.Decode exposing (Decoder, decodeString, errorToString)
import Result exposing (mapError)


port akirejo : (( String, String ) -> msg) -> Sub msg


port konservu : ( String, String ) -> Cmd msg


port akiru : String -> Cmd msg


port raportiErar : String -> Cmd msg


atend : (String -> Result String x) -> Response String -> Result String x
atend conv resp =
    case resp of
        Http.GoodStatus_ _ tekst ->
            conv tekst

        Http.BadStatus_ _ erar ->
            Err erar

        Http.NetworkError_ ->
            Err "KONEKT"

        _ ->
            Debug.todo ""


atendJson : Decoder a -> Response String -> Result String a
atendJson malkodil =
    atend (mapError errorToString << decodeString malkodil)


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
    | Pigre (() -> Msg)
    | NulMsg


type Malferm
    = Ferm
    | MalfermUzMenu


type NunaAgoMsg
    = AuxMsg NAAuxMsg


type NAAuxMsg
    = AuxAdr String
    | AuxEnsalutu
    | AuxEnsalutRes (Result String ())


type PagxMsg
    = KontKreMsg KontKreMsg


type KontKreMsg
    = UzantNom String
    | UzantBild File
    | UzantBildUrl String
    | UzantPri String
    | LatinigVort (Result String (List ( String, String )))
