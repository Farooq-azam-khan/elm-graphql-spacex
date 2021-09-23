-- Do not manually edit this file, it was auto-generated by dillonkearns/elm-graphql
-- https://github.com/dillonkearns/elm-graphql


module SpaceX.Object.Launch exposing (..)

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
details : SelectionSet (Maybe String) SpaceX.Object.Launch
details =
    Object.selectionForField "(Maybe String)" "details" [] (Decode.string |> Decode.nullable)


{-| -}
id : SelectionSet (Maybe SpaceX.ScalarCodecs.Id) SpaceX.Object.Launch
id =
    Object.selectionForField "(Maybe ScalarCodecs.Id)" "id" [] (SpaceX.ScalarCodecs.codecs |> SpaceX.Scalar.unwrapCodecs |> .codecId |> .decoder |> Decode.nullable)


{-| -}
is_tentative : SelectionSet (Maybe Bool) SpaceX.Object.Launch
is_tentative =
    Object.selectionForField "(Maybe Bool)" "is_tentative" [] (Decode.bool |> Decode.nullable)


{-| -}
launch_date_local : SelectionSet (Maybe SpaceX.ScalarCodecs.Date) SpaceX.Object.Launch
launch_date_local =
    Object.selectionForField "(Maybe ScalarCodecs.Date)" "launch_date_local" [] (SpaceX.ScalarCodecs.codecs |> SpaceX.Scalar.unwrapCodecs |> .codecDate |> .decoder |> Decode.nullable)


{-| -}
launch_date_unix : SelectionSet (Maybe SpaceX.ScalarCodecs.Date) SpaceX.Object.Launch
launch_date_unix =
    Object.selectionForField "(Maybe ScalarCodecs.Date)" "launch_date_unix" [] (SpaceX.ScalarCodecs.codecs |> SpaceX.Scalar.unwrapCodecs |> .codecDate |> .decoder |> Decode.nullable)


{-| -}
launch_date_utc : SelectionSet (Maybe SpaceX.ScalarCodecs.Date) SpaceX.Object.Launch
launch_date_utc =
    Object.selectionForField "(Maybe ScalarCodecs.Date)" "launch_date_utc" [] (SpaceX.ScalarCodecs.codecs |> SpaceX.Scalar.unwrapCodecs |> .codecDate |> .decoder |> Decode.nullable)


{-| -}
launch_site :
    SelectionSet decodesTo SpaceX.Object.LaunchSite
    -> SelectionSet (Maybe decodesTo) SpaceX.Object.Launch
launch_site object____ =
    Object.selectionForCompositeField "launch_site" [] object____ (Basics.identity >> Decode.nullable)


{-| -}
launch_success : SelectionSet (Maybe Bool) SpaceX.Object.Launch
launch_success =
    Object.selectionForField "(Maybe Bool)" "launch_success" [] (Decode.bool |> Decode.nullable)


{-| -}
launch_year : SelectionSet (Maybe String) SpaceX.Object.Launch
launch_year =
    Object.selectionForField "(Maybe String)" "launch_year" [] (Decode.string |> Decode.nullable)


{-| -}
links :
    SelectionSet decodesTo SpaceX.Object.LaunchLinks
    -> SelectionSet (Maybe decodesTo) SpaceX.Object.Launch
links object____ =
    Object.selectionForCompositeField "links" [] object____ (Basics.identity >> Decode.nullable)


{-| -}
mission_id : SelectionSet (Maybe (List (Maybe String))) SpaceX.Object.Launch
mission_id =
    Object.selectionForField "(Maybe (List (Maybe String)))" "mission_id" [] (Decode.string |> Decode.nullable |> Decode.list |> Decode.nullable)


{-| -}
mission_name : SelectionSet (Maybe String) SpaceX.Object.Launch
mission_name =
    Object.selectionForField "(Maybe String)" "mission_name" [] (Decode.string |> Decode.nullable)


{-| -}
rocket :
    SelectionSet decodesTo SpaceX.Object.LaunchRocket
    -> SelectionSet (Maybe decodesTo) SpaceX.Object.Launch
rocket object____ =
    Object.selectionForCompositeField "rocket" [] object____ (Basics.identity >> Decode.nullable)


{-| -}
static_fire_date_unix : SelectionSet (Maybe SpaceX.ScalarCodecs.Date) SpaceX.Object.Launch
static_fire_date_unix =
    Object.selectionForField "(Maybe ScalarCodecs.Date)" "static_fire_date_unix" [] (SpaceX.ScalarCodecs.codecs |> SpaceX.Scalar.unwrapCodecs |> .codecDate |> .decoder |> Decode.nullable)


{-| -}
static_fire_date_utc : SelectionSet (Maybe SpaceX.ScalarCodecs.Date) SpaceX.Object.Launch
static_fire_date_utc =
    Object.selectionForField "(Maybe ScalarCodecs.Date)" "static_fire_date_utc" [] (SpaceX.ScalarCodecs.codecs |> SpaceX.Scalar.unwrapCodecs |> .codecDate |> .decoder |> Decode.nullable)


{-| -}
telemetry :
    SelectionSet decodesTo SpaceX.Object.LaunchTelemetry
    -> SelectionSet (Maybe decodesTo) SpaceX.Object.Launch
telemetry object____ =
    Object.selectionForCompositeField "telemetry" [] object____ (Basics.identity >> Decode.nullable)


{-| -}
tentative_max_precision : SelectionSet (Maybe String) SpaceX.Object.Launch
tentative_max_precision =
    Object.selectionForField "(Maybe String)" "tentative_max_precision" [] (Decode.string |> Decode.nullable)


{-| -}
upcoming : SelectionSet (Maybe Bool) SpaceX.Object.Launch
upcoming =
    Object.selectionForField "(Maybe Bool)" "upcoming" [] (Decode.bool |> Decode.nullable)


{-| -}
ships :
    SelectionSet decodesTo SpaceX.Object.Ship
    -> SelectionSet (Maybe (List (Maybe decodesTo))) SpaceX.Object.Launch
ships object____ =
    Object.selectionForCompositeField "ships" [] object____ (Basics.identity >> Decode.nullable >> Decode.list >> Decode.nullable)