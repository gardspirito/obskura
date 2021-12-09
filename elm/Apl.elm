module Apl exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Cxies exposing (Malferm(..), Msg(..), NAAuxMsg(..), NunaAgoMsg(..), PagxMsg(..), bildFormatoj)
import File.Select exposing (file)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import I18Next exposing (Translations, translationsDecoder)
import Json.Decode
import Json.Encode
import Konservejo exposing (..)
import Konto
import Lingvar as L
import Maybe
import NunaAgo
import Tuple exposing (mapFirst)
import Url



-- FARENDE: Ĉu ne uzu localStorage por konvservado de uzanta retpoŝto?
-- Ĝi ĉiuokaze estas konservata far la krozilo.


main : Program Json.Encode.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , onUrlChange = onUrlChange
        , onUrlRequest = onUrlRequest
        , subscriptions = subscriptions
        , update = update
        }


init : Json.Encode.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init en _ sxlos =
    case Json.Decode.decodeValue translationsDecoder en of
        Ok tr ->
            ( { sxlos = sxlos, l = tr, nunaAgo = Nothing, pagx = KontKreiPagx { nomo = "", id = "", uzantbild = Nothing, uzantbildUrl = Nothing, pri = "" } }, Cmd.none )

        Err _ ->
            Debug.todo "Eraro dum ŝaltado."


onUrlRequest : Browser.UrlRequest -> Msg
onUrlRequest =
    UrlSxan


onUrlChange : Url.Url -> Msg
onUrlChange _ =
    Debug.todo "TODO"


type PagTip
    = PagxRegula
    | KontKreiPagx Konto.Model


type alias Model =
    { sxlos : Nav.Key
    , l : Translations
    , nunaAgo : Maybe NunaAgo.Model
    , pagx : PagTip
    }


akirAlMsg : ( String, String ) -> Msg
akirAlMsg ( nom, val ) =
    case nom of
        "retposxt" ->
            NunaAgoMsg <| AuxMsg <| AuxAdr val

        _ ->
            NulMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg_ model =
    case msg_ of
        NulMsg ->
            ( model, Cmd.none )

        Akir akir ->
            update (akirAlMsg akir) model

        NunaAgoMsg msg ->
            case model.nunaAgo of
                Just m ->
                    mapFirst (\x -> { model | nunaAgo = Just x }) <| NunaAgo.gxis msg m

                Nothing ->
                    ( model, Cmd.none )

        PagxMsg (KontKreMsg msg) ->
            case model.pagx of
                KontKreiPagx m ->
                    mapFirst (\x -> { model | pagx = KontKreiPagx x }) <| Konto.gxis msg m

                _ ->
                    ( model, Cmd.none )

        Malferm mf ->
            case ( mf, model.nunaAgo ) of
                ( MalfermUzMenu, Just (NunaAgo.AuxMod _) ) ->
                    ( model, Cmd.none )

                ( MalfermUzMenu, mortant ) ->
                    ( { model | nunaAgo = Just <| NunaAgo.AuxMod { adr = "", erar = "", respAtend = False } }, Cmd.batch [ mortSignal mortant, akiru "retposxt" ] )

                ( Ferm, mortant ) ->
                    ( { model | nunaAgo = Nothing }, mortSignal mortant )

        UrlSxan (External url) ->
            ( model, Nav.load url )

        UrlSxan (Internal _) ->
            Debug.todo ""

        BildInstal f ->
            ( model, file bildFormatoj f )

        Pigre f ->
            update (f ()) model


mortSignal : Maybe NunaAgo.Model -> Cmd Msg
mortSignal en =
    case en of
        Just (NunaAgo.AuxMod { adr }) ->
            konservu ( "retposxt", adr )

        Just (NunaAgo.AuxSukc _) ->
            Cmd.none

        Nothing ->
            Cmd.none


subscriptions : Model -> Sub Msg
subscriptions _ =
    akirejo Akir


rompNunanAgon : Model -> List (Attribute Msg)
rompNunanAgon model =
    case model.nunaAgo |> Maybe.map NunaAgo.cxuRompebla of
        Just True ->
            [ onClick (Malferm Ferm) ]

        _ ->
            []


view : Model -> Browser.Document Msg
view model =
    case model.pagx of
        PagxRegula ->
            { title = "Testpaĝo"
            , body =
                [ div (rompNunanAgon model)
                    [ div [ id "supr" ]
                        [ div
                            [ id "uzant"
                            , onClick (Malferm MalfermUzMenu)
                            ]
                            (NunaAgo.montrUzantMenu model)
                        ]
                    , div [] lorem
                    ]
                ]
            }

        KontKreiPagx m ->
            { title = L.auxKreiKonton model.l
            , body = Konto.montr m model.l
            }


lorem : List (Html a)
lorem =
    [ p [] [ text "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Duis iaculis nisl a viverra congue. Sed eros ex, luctus sed auctor quis, accumsan eu libero. Nullam dictum egestas dapibus. In sit amet dignissim ligula. Quisque iaculis enim enim, et ultricies nibh aliquam sit amet. Nam eget sollicitudin lectus, eu maximus elit. Cras aliquam consectetur quam id consectetur. Etiam in scelerisque orci. Cras tristique sem dignissim lorem ultricies placerat. Ut id maximus quam. Nulla blandit orci ipsum, id rhoncus tellus auctor vel. Vivamus dapibus, nibh eget gravida efficitur, leo turpis dapibus augue, at accumsan libero eros hendrerit justo. Vestibulum ex eros, rhoncus et nunc non, fermentum ornare turpis. Proin quam leo, vestibulum ac placerat eget, molestie ut ex." ]
    , p [] [ text "Morbi suscipit fermentum felis. Proin condimentum lorem bibendum ante ultricies rhoncus. Vestibulum lectus ligula, lobortis et felis auctor, lobortis tempus nibh. Vivamus et gravida neque, in ultrices ante. Donec aliquam bibendum elit a maximus. Phasellus ac dui eros. Nulla in cursus justo." ]
    , p [] [ text "Sed at mi leo. In lorem odio, consequat vitae turpis sit amet, molestie dignissim magna. Suspendisse a feugiat magna. Morbi egestas a odio vel luctus. Curabitur ac suscipit felis. Donec ut est posuere, malesuada eros non, mollis mi. Nam rutrum cursus orci. Nullam bibendum sagittis orci at convallis. Duis scelerisque risus nec nulla auctor, ut tempor nulla tempor. Maecenas et nulla imperdiet, posuere felis sed, tincidunt massa. Etiam ac lacus quis arcu maximus dapibus et ac lectus. Morbi dignissim ornare efficitur. In interdum, purus et lacinia condimentum, ligula augue suscipit odio, id consectetur libero erat in magna. Nam condimentum eros accumsan, vulputate nisl vel, convallis elit. Aenean ornare porta dui id lacinia. Suspendisse purus mauris, ultricies eu lacus eget, molestie interdum nunc." ]
    , p [] [ text "Donec et sagittis orci, eget condimentum urna. Nulla volutpat augue et massa varius, a malesuada turpis porttitor. Maecenas et velit nec nunc dictum cursus sed quis nisi. Maecenas vitae facilisis magna, at condimentum odio. Praesent fermentum lectus sit amet orci laoreet lobortis. Praesent convallis enim dolor, in porta mi lobortis condimentum. Cras ac lacus erat. Suspendisse volutpat sollicitudin sagittis. Duis ut orci sit amet erat sollicitudin iaculis sed quis dui. Etiam nisi dolor, varius ac enim eget, pharetra bibendum mi. Donec commodo euismod nisi, at semper augue condimentum eu. Sed dignissim maximus viverra. Pellentesque cursus leo non elementum bibendum." ]
    , p [] [ text "Maecenas non arcu consectetur, laoreet nisi malesuada, consectetur urna. Mauris bibendum ex eu metus tempus, vel condimentum urna rutrum. Aenean in odio venenatis, blandit tellus vitae, imperdiet nisi. Sed sit amet eros quis lacus blandit consequat. Duis egestas turpis nec semper venenatis. Etiam arcu libero, rutrum nec velit eu, consectetur ultrices felis. Cras rhoncus sem vel porta finibus. Integer condimentum, nisl sit amet rutrum finibus, nibh nisi accumsan neque, non molestie dolor orci ut ipsum. Etiam suscipit laoreet neque eget sollicitudin." ]
    ]
