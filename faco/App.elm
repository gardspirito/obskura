module App exposing (..)

import Browser
import Browser.Navigation as Nav
import Url
import Html exposing (..)
import Html.Attributes exposing (..)

type Super = Sxargad | 

type alias Model = {
    sxlos : Nav.Key
  }

type Mes = Mes

main : Program () Model Mes
main = Browser.application {
    init = init,
    view = view,
    onUrlChange = onUrlChange,
    onUrlRequest = onUrlRequest,
    subscriptions = subscriptions,
    update = update
  }

onUrlRequest : Browser.UrlRequest -> Mes
onUrlRequest arg1 =
    Debug.todo "TODO"


onUrlChange : Url.Url -> Mes
onUrlChange arg1 =
    Debug.todo "TODO"

update : Mes -> Model -> (Model, Cmd Mes)
update _ model =
    (model, Cmd.none)


subscriptions : Model -> Sub Mes
subscriptions arg1 =
    Debug.todo "TODO"

init : () -> Url.Url -> Nav.Key -> (Model, Cmd Mes)
init _ _ sxlos = ({ sxlos = sxlos }, Cmd.none)

view : Model -> Browser.Document Mes
view _ =
  {
    title = "Testpaĝo",
    body = [text "bla"]
  }