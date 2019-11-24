module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Players exposing (Player, Players)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Model =
    { players : Players
    , newName : String
    , newAvatar : String
    }


init : Model
init =
    { players =
        Players.empty
            |> Players.addPlayer "Torbjørn" "https://avatars2.githubusercontent.com/u/191559?s=400&v=4"
            |> Players.addPlayer "Robert" "https://avatars2.githubusercontent.com/u/6553283?s=460&v=4"
    , newName = ""
    , newAvatar = ""
    }



-- UPDATE


type Msg
    = Increment Int
    | InputName String
    | InputAvatar String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment id ->
            { model | players = Players.addToPlayerScore id 2 model.players }

        InputName input ->
            { model | newName = input }

        InputAvatar input ->
            { model | newAvatar = input }

        Submit ->
            if String.length model.newAvatar > 0 && String.length model.newName > 0 then
                { model
                    | newAvatar = ""
                    , newName = ""
                    , players = Players.addPlayer model.newName model.newAvatar model.players
                }

            else
                model



-- VIEW


view : Model -> Html Msg
view model =
    div [ class "pure-g" ]
        [ div [ class "pure-u-1" ] [ h1 [] [ text "Ka e stillingen" ] ]
        , div [ class "pure-u-3-5" ]
            [ div
                [ class "pure-g" ]
                (List.map
                    renderPlayer
                    (Players.sorted
                        model.players
                    )
                )
            ]
        , div [ class "pure-u-2-5" ] [ renderNewPlayerForm model ]
        ]


renderPlayer : Player -> Html Msg
renderPlayer player =
    div [ class "pure-u-1" ]
        [ div [ class "pure-g box" ]
            [ div [ class "pure-u-1-4" ] [ img [ src player.avatar, width 50, height 50, class "pure-img" ] [] ]
            , div [ class "pure-u-1-4" ] [ text player.name ]
            , div [ class "pure-u-1-4" ] [ text (String.fromInt player.score) ]
            , div [ class "pure-u-1-4" ] [ button [ onClick (Increment player.id) ] [ text "+" ] ]
            ]
        ]


renderNewPlayerForm : Model -> Html Msg
renderNewPlayerForm model =
    div [ class "pure-g" ]
        [ div [ class "pure-u-1" ]
            [ h3 [] [ text "Ny spiller" ] ]
        , div
            [ class "pure-u-1" ]
            [ p [] [ input [ type_ "text", placeholder "Navn", onInput InputName, value model.newName ] [] ]
            , p [] [ input [ type_ "text", placeholder "Avatar", onInput InputAvatar, value model.newAvatar ] [] ]
            , p [] [ button [ onClick Submit ] [ text "Legg til" ] ]
            ]
        ]
