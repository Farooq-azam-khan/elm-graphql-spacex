-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SpaceX.Object.Info exposing (..)

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
ceo : SelectionSet (Maybe String) SpaceX.Object.Info
ceo =
    Object.selectionForField "(Maybe String)" "ceo" [] (Decode.string |> Decode.nullable)


{-| -}
coo : SelectionSet (Maybe String) SpaceX.Object.Info
coo =
    Object.selectionForField "(Maybe String)" "coo" [] (Decode.string |> Decode.nullable)


{-| -}
cto_propulsion : SelectionSet (Maybe String) SpaceX.Object.Info
cto_propulsion =
    Object.selectionForField "(Maybe String)" "cto_propulsion" [] (Decode.string |> Decode.nullable)


{-| -}
cto : SelectionSet (Maybe String) SpaceX.Object.Info
cto =
    Object.selectionForField "(Maybe String)" "cto" [] (Decode.string |> Decode.nullable)


{-| -}
employees : SelectionSet (Maybe Int) SpaceX.Object.Info
employees =
    Object.selectionForField "(Maybe Int)" "employees" [] (Decode.int |> Decode.nullable)


{-| -}
founded : SelectionSet (Maybe Int) SpaceX.Object.Info
founded =
    Object.selectionForField "(Maybe Int)" "founded" [] (Decode.int |> Decode.nullable)


{-| -}
founder : SelectionSet (Maybe String) SpaceX.Object.Info
founder =
    Object.selectionForField "(Maybe String)" "founder" [] (Decode.string |> Decode.nullable)


{-| -}
headquarters :
    SelectionSet decodesTo SpaceX.Object.Address
    -> SelectionSet (Maybe decodesTo) SpaceX.Object.Info
headquarters object____ =
    Object.selectionForCompositeField "headquarters" [] object____ (Basics.identity >> Decode.nullable)


{-| -}
launch_sites : SelectionSet (Maybe Int) SpaceX.Object.Info
launch_sites =
    Object.selectionForField "(Maybe Int)" "launch_sites" [] (Decode.int |> Decode.nullable)


{-| -}
links :
    SelectionSet decodesTo SpaceX.Object.InfoLinks
    -> SelectionSet (Maybe decodesTo) SpaceX.Object.Info
links object____ =
    Object.selectionForCompositeField "links" [] object____ (Basics.identity >> Decode.nullable)


{-| -}
name : SelectionSet (Maybe String) SpaceX.Object.Info
name =
    Object.selectionForField "(Maybe String)" "name" [] (Decode.string |> Decode.nullable)


{-| -}
summary : SelectionSet (Maybe String) SpaceX.Object.Info
summary =
    Object.selectionForField "(Maybe String)" "summary" [] (Decode.string |> Decode.nullable)


{-| -}
test_sites : SelectionSet (Maybe Int) SpaceX.Object.Info
test_sites =
    Object.selectionForField "(Maybe Int)" "test_sites" [] (Decode.int |> Decode.nullable)


{-| -}
valuation : SelectionSet (Maybe Float) SpaceX.Object.Info
valuation =
    Object.selectionForField "(Maybe Float)" "valuation" [] (Decode.float |> Decode.nullable)


{-| -}
vehicles : SelectionSet (Maybe Int) SpaceX.Object.Info
vehicles =
    Object.selectionForField "(Maybe Int)" "vehicles" [] (Decode.int |> Decode.nullable)