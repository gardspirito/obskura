port module Konservejo exposing (..)


port akirejo : (( String, String ) -> msg) -> Sub msg


port konservu : ( String, String ) -> Cmd msg


port akiru : String -> Cmd msg
