module Main exposing(main)

import Browser
import Graphql.Operation exposing (RootQuery)
import Graphql.Http
import Graphql.SelectionSet as SelectionSet exposing (SelectionSet)
import Graphql.Document as GqlDoc 
import Graphql.OptionalArgument exposing (OptionalArgument(..))

import Html 
import Html.Attributes as HA
import RemoteData exposing (RemoteData)

import SpaceX.Query as Query
import SpaceX.Object
import SpaceX.Object.Capsule as Capsule
import SpaceX.ScalarCodecs exposing (Id) 

type Msg = GotResponse Model 
type alias Model = RemoteData (Graphql.Http.Error Response) Response

type alias Flags = {}
init : Flags -> (Model, Cmd Msg)
init _ = (RemoteData.Loading, makeRequest)

update :  Msg -> Model -> (Model, Cmd Msg)
update  msg model =
    case msg of 
        GotResponse response -> 
            (response, Cmd.none)

display_capsules :  Maybe (List (Maybe CapsuleData)) -> Html.Html Msg 
display_capsules capsule_resp =
    case capsule_resp of 
        Just capsule -> 
            Html.ol 
                [] 
                (List.map 
                    (\cap -> 
                        Html.li 
                            [] 
                            [ case cap of
                                Just x -> Html.text <| Debug.toString x
                                Nothing -> Html.text ""
                            ]) 
                    capsule
                )
        _ -> Html.text ""


view : Model -> Html.Html Msg 
view model = 
    Html.div 
        [] 
        [ Html.h1  []  [Html.text "Generate Query"]
        , Html.pre [] [Html.text <| GqlDoc.serializeQuery query]
        , Html.h1 [] [Html.text "Response"]
        , case model of 
            RemoteData.Success response -> 
                display_capsules response.capsules
            _ -> Html.text ""
        ]
    
main : Program Flags Model Msg 
main = 
    Browser.element 
        { init = init
        , view = view 
        , update = update 
        , subscriptions = sub
        }

sub : Model -> Sub Msg 
sub _ = Sub.none 




request_api : String 
request_api = "https://api.spacex.land/graphql/"
makeRequest : Cmd Msg 
makeRequest = 
    query |> Graphql.Http.queryRequest request_api
          |> Graphql.Http.send (RemoteData.fromResult >> GotResponse)


type alias CapsuleData = 
    { id : Maybe Id
    , landings: Maybe Int 
    , reuse_count: Maybe Int 
    , status: Maybe String
    -- , original_launch: String 
    }
type alias Response = {capsules : Maybe (List (Maybe CapsuleData))}
-- capsules_args : CapsulesOptionalArguments
-- capsules_args = 
--     { find : OptionalArgument SpaceX.InputObject.CapsulesFind
--     , limit : OptionalArgument Int
--     , offset : OptionalArgument Int
--     , order : OptionalArgument String
--     , sort : OptionalArgument String
--     }
query : SelectionSet Response RootQuery
query = SelectionSet.map Response 
                        (Query.capsules (\options -> {options | limit = Present 10} ) capsuleSelection)


capsuleSelection : SelectionSet CapsuleData SpaceX.Object.Capsule
capsuleSelection = SelectionSet.map4 CapsuleData Capsule.id Capsule.landings Capsule.reuse_count Capsule.status 