module Main exposing (..)

import Browser
import Html exposing (Html, button, div, li, p, span, strong, text, ul)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, list, string)



--Model


type alias Model =
    { topics : List Topic
    , current_user : User
    }


type alias User =
    { id : Int
    , first_name : String
    , last_name : String
    }



-- type alias TopicSubscribed =
--     { user_id : String }


type alias Topic =
    { name : String

    -- , subscribed : List TopicSubscribed
    }



-- API Decoders


init : () -> ( Model, Cmd Msg )
init _ =
    ( { topics =
            [ Topic "topic1"
            , Topic "topic2"
            ]
      , current_user = User 12 "CHat" "User"
      }
    , fetchTopicsCmd
    )



-- Main


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type Msg
    = Subscribe Topic User
    | Success
    | GetTopicsCompleted (Result Http.Error (List Topic))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Success ->
            ( model, Cmd.none )

        Subscribe topic user ->
            ( model, Cmd.none )

        GetTopicsCompleted result ->
            case result of
                Ok newTopics ->
                    ( { model | topics = newTopics }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div [ class "column" ]
            [ ul [] <| List.map (viewTopicWithUser model.current_user) model.topics
            ]
        , div [ class "column" ]
            [ text "some text"
            ]
        ]



-- VIEW HELPERS


viewRecord : User -> Html Msg
viewRecord user =
    li []
        [ strong [] [ text (full_name user) ]
        ]


viewTopic : User -> Topic -> Html Msg
viewTopic currentUser topic =
    li []
        [ div []
            [ p [ class "container" ]
                [ text topic.name
                , span [ class "topic-button-right" ]
                    [ button [ onClick (Subscribe topic currentUser) ] [ text "Subscribe" ]
                    ]
                ]
            ]
        ]


viewTopicWithUser : User -> (Topic -> Html Msg)
viewTopicWithUser user =
    viewTopic user



-- UTILS


full_name : User -> String
full_name user =
    user.first_name ++ " " ++ user.last_name



-- REST API functions


api : String
api =
    "http://localhost:8080"


fetchTopicsCmd : Cmd Msg
fetchTopicsCmd =
    Http.get
        { url = api ++ "/topics"
        , expect = Http.expectJson GetTopicsCompleted topicListDecoder
        }



-- Decoders
-- subscibedDecoder : Decoder TopicSubscribed
-- subscibedDecoder =
--     Decode.map3 TopicSubscribed
--         (field "user_id" string)


topicDecoder : Decoder Topic
topicDecoder =
    Decode.map Topic
        (field "topic" string)



-- ( field "subscribed", Decode.list subscibedDecoder )


topicListDecoder : Decoder (List Topic)
topicListDecoder =
    Decode.list topicDecoder
