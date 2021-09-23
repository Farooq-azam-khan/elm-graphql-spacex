-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SpaceX.Object.RocketSecondStage exposing (..)

import Graphql.Internal.Builder.Argument as Argument exposing (Argument)
import Graphql.Internal.Builder.Object as Object
import Graphql.Internal.Encode as Encode exposing (Value)
import Graphql.Operation exposing (RootMutation, RootQuery, RootSubscription)
import Graphql.OptionalArgument exposing (OptionalArgument(..))
import Graphql.SelectionSet exposing (SelectionSet)
import Json.Decode as Decode
import SpaceX.InputObject
import SpaceX.Interface
import SpaceX.Object
import SpaceX.Scalar
import SpaceX.ScalarCodecs
import SpaceX.Union


{-| -}
burn_time_sec : SelectionSet (Maybe Int) SpaceX.Object.RocketSecondStage
burn_time_sec =
    Object.selectionForField "(Maybe Int)" "burn_time_sec" [] (Decode.int |> Decode.nullable)


{-| -}
engines : SelectionSet (Maybe Int) SpaceX.Object.RocketSecondStage
engines =
    Object.selectionForField "(Maybe Int)" "engines" [] (Decode.int |> Decode.nullable)


{-| -}
fuel_amount_tons : SelectionSet (Maybe Float) SpaceX.Object.RocketSecondStage
fuel_amount_tons =
    Object.selectionForField "(Maybe Float)" "fuel_amount_tons" [] (Decode.float |> Decode.nullable)


{-| -}
payloads :
    SelectionSet decodesTo SpaceX.Object.RocketSecondStagePayloads
    -> SelectionSet (Maybe decodesTo) SpaceX.Object.RocketSecondStage
payloads object____ =
    Object.selectionForCompositeField "payloads" [] object____ (Basics.identity >> Decode.nullable)


{-| -}
thrust :
    SelectionSet decodesTo SpaceX.Object.Force
    -> SelectionSet (Maybe decodesTo) SpaceX.Object.RocketSecondStage
thrust object____ =
    Object.selectionForCompositeField "thrust" [] object____ (Basics.identity >> Decode.nullable)
