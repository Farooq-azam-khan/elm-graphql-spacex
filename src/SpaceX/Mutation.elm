-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SpaceX.Mutation exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode exposing (Decoder)
import SpaceX.InputObject
import SpaceX.Interface
import SpaceX.Object
import SpaceX.Scalar
import SpaceX.ScalarCodecs
import SpaceX.Union


type alias DeleteUsersRequiredArguments =
    { where_ : SpaceX.InputObject.Users_bool_exp }


{-| delete data from the table: "users"

  - where\_ - filter the rows which have to be deleted

-}
delete_users :
    DeleteUsersRequiredArguments
    -> SelectionSet decodesTo SpaceX.Object.Users_mutation_response
    -> SelectionSet (Maybe decodesTo) RootMutation
delete_users requiredArgs____ object____ =
    Object.selectionForCompositeField "delete_users" [ Argument.required "where" requiredArgs____.where_ SpaceX.InputObject.encodeUsers_bool_exp ] object____ (Basics.identity >> Decode.nullable)


type alias InsertUsersOptionalArguments =
    { on_conflict : OptionalArgument SpaceX.InputObject.Users_on_conflict }


type alias InsertUsersRequiredArguments =
    { objects : List SpaceX.InputObject.Users_insert_input }


{-| insert data into the table: "users"

  - objects - the rows to be inserted
  - on\_conflict - on conflict condition

-}
insert_users :
    (InsertUsersOptionalArguments -> InsertUsersOptionalArguments)
    -> InsertUsersRequiredArguments
    -> SelectionSet decodesTo SpaceX.Object.Users_mutation_response
    -> SelectionSet (Maybe decodesTo) RootMutation
insert_users fillInOptionals____ requiredArgs____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { on_conflict = Absent }

        optionalArgs____ =
            [ Argument.optional "on_conflict" filledInOptionals____.on_conflict SpaceX.InputObject.encodeUsers_on_conflict ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "insert_users" (optionalArgs____ ++ [ Argument.required "objects" requiredArgs____.objects (SpaceX.InputObject.encodeUsers_insert_input |> Encode.list) ]) object____ (Basics.identity >> Decode.nullable)


type alias UpdateUsersOptionalArguments =
    { set_ : OptionalArgument SpaceX.InputObject.Users_set_input }


type alias UpdateUsersRequiredArguments =
    { where_ : SpaceX.InputObject.Users_bool_exp }


{-| update data of the table: "users"

  - set\_ - sets the columns of the filtered rows to the given values
  - where\_ - filter the rows which have to be updated

-}
update_users :
    (UpdateUsersOptionalArguments -> UpdateUsersOptionalArguments)
    -> UpdateUsersRequiredArguments
    -> SelectionSet decodesTo SpaceX.Object.Users_mutation_response
    -> SelectionSet (Maybe decodesTo) RootMutation
update_users fillInOptionals____ requiredArgs____ object____ =
    let
        filledInOptionals____ =
            fillInOptionals____ { set_ = Absent }

        optionalArgs____ =
            [ Argument.optional "_set" filledInOptionals____.set_ SpaceX.InputObject.encodeUsers_set_input ]
                |> List.filterMap Basics.identity
    in
    Object.selectionForCompositeField "update_users" (optionalArgs____ ++ [ Argument.required "where" requiredArgs____.where_ SpaceX.InputObject.encodeUsers_bool_exp ]) object____ (Basics.identity >> Decode.nullable)
