module Mesagxoj exposing (..)


type Msg
    = NunaAgoMsg NunaAgoMsg
    | Malferm Malferm
    | Akir ( String, String )
    | Nul


type Malferm
    = Ferm
    | MalfermUzMenu


type NunaAgoMsg
    = AuxMsg NAAuxMsg


type NAAuxMsg
    = AuxAdr String
    | AuxEnsalutu
