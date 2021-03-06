-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SpaceX.Object.RocketSecondStagePayloadCompositeFairing exposing (..)

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
height :
    SelectionSet decodesTo SpaceX.Object.Distance
    -> SelectionSet (Maybe decodesTo) SpaceX.Object.RocketSecondStagePayloadCompositeFairing
height object____ =
    Object.selectionForCompositeField "height" [] object____ (Basics.identity >> Decode.nullable)


{-| -}
diameter :
    SelectionSet decodesTo SpaceX.Object.Distance
    -> SelectionSet (Maybe decodesTo) SpaceX.Object.RocketSecondStagePayloadCompositeFairing
diameter object____ =
    Object.selectionForCompositeField "diameter" [] object____ (Basics.identity >> Decode.nullable)
