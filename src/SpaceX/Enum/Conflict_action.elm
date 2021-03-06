-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SpaceX.Enum.Conflict_action exposing (..)

import Json.Decode as Decode exposing (Decoder)


{-| conflict action

  - Ignore - ignore the insert on this row
  - Update - update the row with the given values

-}
type Conflict_action
    = Ignore
    | Update


list : List Conflict_action
list =
    [ Ignore, Update ]


decoder : Decoder Conflict_action
decoder =
    Decode.string
        |> Decode.andThen
            (\string ->
                case string of
                    "ignore" ->
                        Decode.succeed Ignore

                    "update" ->
                        Decode.succeed Update

                    _ ->
                        Decode.fail ("Invalid Conflict_action type, " ++ string ++ " try re-running the @dillonkearns/elm-graphql CLI ")
            )


{-| Convert from the union type representing the Enum to a string that the GraphQL server will recognize.
-}
toString : Conflict_action -> String
toString enum____ =
    case enum____ of
        Ignore ->
            "ignore"

        Update ->
            "update"


{-| Convert from a String representation to an elm representation enum.
This is the inverse of the Enum `toString` function. So you can call `toString` and then convert back `fromString` safely.

    Swapi.Enum.Episode.NewHope
        |> Swapi.Enum.Episode.toString
        |> Swapi.Enum.Episode.fromString
        == Just NewHope

This can be useful for generating Strings to use for <select> menus to check which item was selected.

-}
fromString : String -> Maybe Conflict_action
fromString enumString____ =
    case enumString____ of
        "ignore" ->
            Just Ignore

        "update" ->
            Just Update

        _ ->
            Nothing
