module Players exposing (Player, Players, addPlayer, addToPlayerScore, empty, sorted)


type alias Player =
    { id : Int
    , name : String
    , avatar : String
    , score : Int
    }


type Players
    = Players (List Player)


empty : Players
empty =
    Players []


addPlayer : String -> String -> Players -> Players
addPlayer name avatar p =
    case p of
        Players players ->
            Players
                (Player (List.length players) name avatar 0
                    :: players
                )


sorted : Players -> List Player
sorted p =
    case p of
        Players players ->
            List.sortBy .score players |> List.reverse


addToPlayerScore : Int -> Int -> Players -> Players
addToPlayerScore id value p =
    case p of
        Players players ->
            Players
                (players
                    |> List.map
                        (\item ->
                            if item.id == id then
                                { item | score = item.score + value }

                            else
                                item
                        )
                )
