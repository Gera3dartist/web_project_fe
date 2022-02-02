port module Main exposing (..)

import Browser
import Browser.Navigation as Nav
import Debug
import Html exposing (Html, a, b, button, div, h1, input, li, p, span, strong, text, textarea, ul)
import Html.Attributes exposing (class, href, id, placeholder, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as Decode exposing (Decoder, field, int, list, string)
import Url exposing (Url)



--Model


type alias Model =
    { topics : List Topic
    , current_user : User
    , subscribed_topics : List Topic
    , currentTopic : String
    , draft : String
    , messages : List ChatMessage
    , key : Nav.Key
    , url : Url.Url
    , currentPage : CurrentPage
    , users : List User
    }


type alias User =
    { id : Int
    , first_name : String
    , last_name : String
    }


type alias Topic =
    { name : String
    }


type alias TopicMessages =
    { name : String
    , messages : List String
    }


type alias ChatMessage =
    { payload : String
    , topic : String
    , userid : String
    , username : String
    }



-- API Decoders


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        model =
            { topics = []
            , current_user = User 1 "Joe" "Armstrong"
            , subscribed_topics = []
            , draft = ""
            , currentTopic = "elm"
            , messages = []
            , url = url
            , key = key
            , currentPage = Login
            , users = []
            }
    in
    ( model
    , Cmd.batch
        [ fetchTopicsCmd model
        , fetchUserTopicsCmd model
        , fetchUsersCmd model
        ]
    )


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = LinkClicked
        }



-- PORTS


port sendMessage : List String -> Cmd msg


port messageReceiver : (String -> msg) -> Sub msg


port connectUser : Int -> Cmd msg



-- SUBSCRIPTIONS
-- subscribe to the  messageReceiver port to hear about incomming messages
--


subscriptions : Model -> Sub Msg
subscriptions model =
    messageReceiver Recv


type CurrentPage
    = Login
    | Chat



-- MSG


type Msg
    = Subscribe Topic User
    | Success
    | DraftChanged String
    | Send
    | UserLoggedIn User
    | ConnectUser
    | Recv String
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GetTopicsCompleted (Result Http.Error (List Topic))
    | GetUserTopicsCompleted (Result Http.Error (List Topic))
    | GetUsersCompleted (Result Http.Error (List User))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DraftChanged draft ->
            ( { model | draft = draft }, Cmd.none )

        Success ->
            ( model, Cmd.none )

        Subscribe topic user ->
            ( model, Cmd.none )

        ConnectUser ->
            ( model, connectUserSocket model.current_user.id )

        Send ->
            ( { model | draft = "" }, sendMessage [ model.currentTopic, model.draft, full_name model.current_user ] )

        Recv message ->
            case Decode.decodeString chatDecoder message of
                Ok validMessage ->
                    ( { model | messages = model.messages ++ [ validMessage ] }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GetTopicsCompleted result ->
            case result of
                Ok newTopics ->
                    ( { model | topics = newTopics }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GetUserTopicsCompleted result ->
            case result of
                Ok userTopics ->
                    ( { model | subscribed_topics = userTopics }, Cmd.none )

                Err exc ->
                    Debug.log ("foo bar" ++ Debug.toString exc) <| ( model, Cmd.none )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged newUrl ->
            ( { model | url = newUrl }, Cmd.none )

        GetUsersCompleted result ->
            case result of
                Ok newUsers ->
                    ( { model | users = newUsers }, Cmd.none )

                Err exc ->
                    Debug.log ("Problem getting users" ++ Debug.toString exc) <| ( model, Cmd.none )

        UserLoggedIn user ->
            Debug.log ("Connecting User " ++ Debug.toString user) <| ( { model | current_user = user, currentPage = Chat }, connectUserSocket user.id )


view : Model -> Browser.Document Msg
view model =
    case model.currentPage of
        Login ->
            pageTemplate [ viewLoginPage model ]

        Chat ->
            pageTemplate (viewChatHistoryPage model)


pageTemplate : List (Html Msg) -> Browser.Document Msg
pageTemplate body =
    { title = "Chat App"
    , body = body
    }



-- viewLoginPage : Model -> Html Msg


viewChatHistoryPage : Model -> List (Html Msg)
viewChatHistoryPage model =
    [ div []
        [ viewCurrentUser model
        , viewChatHistory model
        ]
    , div
        []
        [ viewChatControl model ]
    ]


viewLoginPage : Model -> Html Msg
viewLoginPage model =
    ul []
        (List.map (\user -> li [ onClick (UserLoggedIn user) ] [ text (full_name user) ]) model.users)


viewLink : String -> Html msg
viewLink path =
    li [] [ a [ href path ] [ text path ] ]



-- VIEW HELPERS


viewSubscribedTopic : Topic -> Html Msg
viewSubscribedTopic topic =
    li []
        [ div []
            [ p [ class "container" ]
                [ text topic.name ]
            ]
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


viewCurrentUser : Model -> Html Msg
viewCurrentUser model =
    div
        [ id (String.fromInt model.current_user.id)
        ]
        [ p []
            [ text (full_name model.current_user)
            ]
        , button [ onClick ConnectUser ] [ text "Connect" ]
        ]


viewChatHistory : Model -> Html Msg
viewChatHistory model =
    div [] [ div [ class "list" ] (List.map (viewChatHistoryRecord model) model.messages) ]


viewChatControl : Model -> Html Msg
viewChatControl model =
    div [ class "chat-control" ]
        [ textarea
            [ placeholder "Draft"
            , onInput DraftChanged
            , value model.draft
            , class "textarea"
            ]
            []
        , button [ onClick Send, class "button" ] [ text "Send" ]
        ]


viewChatHistoryRecord : Model -> ChatMessage -> Html Msg
viewChatHistoryRecord model chatMessage =
    div [ class (getMessageClass model chatMessage.username) ]
        [ span [] [ text (String.left 2 chatMessage.username) ]
        , text chatMessage.payload
        ]



-- UTILS


getMessageClass : Model -> String -> String
getMessageClass model username =
    if full_name model.current_user == username then
        "list-item right"

    else
        "list-item left"


full_name : User -> String
full_name user =
    user.first_name ++ " " ++ user.last_name



-- REST API functions


api : String
api =
    "http://localhost:8080"


fetchTopicsCmd : Model -> Cmd Msg
fetchTopicsCmd model =
    Http.get
        { url = api ++ "/topics"
        , expect = Http.expectJson GetTopicsCompleted topicListDecoder
        }


fetchUserTopicsCmd : Model -> Cmd Msg
fetchUserTopicsCmd model =
    Http.get
        { url = api ++ "/users/" ++ String.fromInt model.current_user.id ++ "/topics"
        , expect = Http.expectJson GetUserTopicsCompleted topicListDecoder
        }


fetchUsersCmd : Model -> Cmd Msg
fetchUsersCmd model =
    Http.get
        { url = api ++ "/users"
        , expect = Http.expectJson GetUsersCompleted userListDecoder
        }



-- DECODERS


topicDecoder : Decoder Topic
topicDecoder =
    Decode.map Topic
        (field "name" string)


topicListDecoder : Decoder (List Topic)
topicListDecoder =
    Decode.list topicDecoder



-- User Decoders


userDecoder : Decoder User
userDecoder =
    Decode.map3 User
        (field "id" int)
        (field "first_name" string)
        (field "last_name" string)


userListDecoder : Decoder (List User)
userListDecoder =
    Decode.list userDecoder



-- Chat Message Decode
-- "{\"payload\":\"chat 1\",\"topic\":\"elm\",\"userid\":\"2\"}"


chatDecoder : Decoder ChatMessage
chatDecoder =
    Decode.map4 ChatMessage
        (field "payload" string)
        (field "topic" string)
        (field "userid" string)
        (field "username" string)


connectUserSocket : Int -> Cmd msg
connectUserSocket user_id =
    connectUser user_id



-- DETECT ENTER
-- ifIsEnter : msg -> Decoder msg
-- ifIsEnter msg =
--     field "key" string
--         |> andThen
--             (\key ->
--                 if key == "Enter" then
--                     D.succeed msg
--                 else
--                     D.fail "some other key"
--             )
