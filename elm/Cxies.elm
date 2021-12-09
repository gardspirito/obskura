module Cxies exposing (..)

import Browser exposing (UrlRequest(..))
import File exposing (File)


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
