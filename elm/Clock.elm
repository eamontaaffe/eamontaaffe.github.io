module Clock       exposing (main)

import Html        exposing (Html, span, text)
import Time        exposing (Time, second)
import Date
import Time.Format exposing (format)
import Date.Extra


main =
    Html.program
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }

-- MODEL


type alias Model =
    { maybeTime : Maybe Time
    , utcOffset : Float
    , zoneCode  : String
    }


init : (Model, Cmd Msg)
init =
    ({ maybeTime = Nothing
     , utcOffset = 10 * 60 * 60 * 1000, zoneCode = "AEST"
     }, Cmd.none)


-- UPDATE


type Msg
    = Tick Time


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        Tick newTime ->
            ({ model | maybeTime = Just newTime }, Cmd.none)


-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions mode =
    Time.every second Tick


-- VIEW


view : Model -> Html Msg
view { maybeTime, utcOffset, zoneCode } =
    case maybeTime of
        Just time ->
            span []
                [ text (toTimeString time utcOffset zoneCode)
                ]
        Nothing ->
            span [] [ text "__:__:__ ____" ]
    
    

toTimeString : Time -> Float -> String -> String
toTimeString time desiredOffset zoneCode =
    let
        currentOffset =
            (Date.Extra.offsetFromUtc (Date.fromTime time)) * 60 * 1000
                |> toFloat

        desiredTime = time - currentOffset + desiredOffset

    in
        (format "%H:%M:%S " desiredTime) ++ zoneCode
    
