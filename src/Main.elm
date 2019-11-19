module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)



-- MAIN


main =
    Browser.sandbox { init = init, update = update, view = view }



-- MODEL


type alias Player =
    { id : Int
    , name : String
    , avatar : String
    , score : Int
    }


type alias Model =
    { players : List Player
    , newName : String
    , newAvatar : String
    }


init : Model
init =
    { players =
        [ Player 0 "Robert" "https://avatars2.githubusercontent.com/u/6553283?s=460&v=4" 10
        , Player 1 "Torbjørn" "https://avatars2.githubusercontent.com/u/191559?s=400&v=4" 5
        ]
    , newName = ""
    , newAvatar = ""
    }



-- UPDATE


type Msg
    = Increment Int
    | TypeName String
    | TypeAvatar String
    | Submit


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment id ->
            { model | players = List.map (updateModelScore id 2) model.players }

        TypeName input ->
            { model | newName = input }

        TypeAvatar input ->
            { model | newAvatar = input }

        Submit ->
            if String.length model.newAvatar > 0 && String.length model.newName > 0 then
                { model
                    | newAvatar = ""
                    , newName = ""
                    , players = Player (List.length model.players) model.newName model.newAvatar 0 :: model.players
                }

            else
                model


updateModelScore id value item =
    if item.id == id then
        { item | score = item.score + value }

    else
        item



-- VIEW


view : Model -> Html Msg
view model =
    let
        sorted =
            List.reverse
                (List.sortBy
                    .score
                    model.players
                )
    in
    div [ class "pure-g" ]
        [ div [ class "pure-u-2-3" ]
            (div [ class "pure-g box" ]
                [ h3 [ class "pure-u-1-1 center" ] [ text "Stilling" ]
                ]
                :: List.map renderPlayer sorted
            )
        , div [ class "pure-u-1-3" ] [ p [] [ renderNewPlayerForm model ] ]
        ]


renderPlayer player =
    div [ class "pure-g box" ]
        [ div [ class "pure-u-1-3" ] [ img [ src player.avatar, width 50, height 50, class "pure-img" ] [] ]
        , div [ class "pure-u-1-3" ] [ text (player.name ++ " -> " ++ String.fromInt player.score) ]
        , div [ class "pure-u-1-3" ] [ button [ onClick (Increment player.id) ] [ text "+" ] ]
        ]


renderNewPlayerForm model =
    div [ class "pure-g box" ]
        [ h3 [ class "pure-u center" ]
            [ text "Ny spiller" ]
        , div
            [ class "pure-u" ]
            [ p [] [ input [ type_ "text", placeholder "Navn", onInput TypeName, value model.newName ] [] ]
            , p [] [ input [ type_ "text", placeholder "Avatar", onInput TypeAvatar, value model.newAvatar ] [] ]
            , p [] [ button [ onClick Submit ] [ text "Legg til" ] ]
            ]
        ]
