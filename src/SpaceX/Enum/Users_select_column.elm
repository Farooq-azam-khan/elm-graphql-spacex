-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SpaceX.Enum.Users_select_column exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| select columns of table "users"

  - Id - column name
  - Name - column name
  - Rocket - column name
  - Timestamp - column name
  - Twitter - column name

-}
type Users_select_column
    = Id
    | Name
    | Rocket
    | Timestamp
    | Twitter


list : List Users_select_column
list =
    [ Id, Name, Rocket, Timestamp, Twitter ]


decoder : Decoder Users_select_column
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "id" ->
                        Decode.succeed Id

                    "name" ->
                        Decode.succeed Name

                    "rocket" ->
                        Decode.succeed Rocket

                    "timestamp" ->
                        Decode.succeed Timestamp

                    "twitter" ->
                        Decode.succeed Twitter

                    _ ->
                        Decode.fail ("Invalid Users_select_column type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : Users_select_column -> String
toString enum____ =
    case enum____ of
        Id ->
            "id"

        Name ->
            "name"

        Rocket ->
            "rocket"

        Timestamp ->
            "timestamp"

        Twitter ->
            "twitter"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Users_select_column
fromString enumString____ =
    case enumString____ of
        "id" ->
            Just Id

        "name" ->
            Just Name

        "rocket" ->
            Just Rocket

        "timestamp" ->
            Just Timestamp

        "twitter" ->
            Just Twitter

        _ ->
            Nothing
