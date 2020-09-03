module Remote.Recyclable exposing
    ( Recyclable(..), SubState(..), GraphqlHttpRecyclable
    , firstLoading
    , mergeResponse, toLoading, fromResponse
    , isReady, isError, isCustomError, isTransportError, isLoading, isNeverAsked
    , getError
    , map, mapCustomError, mapTransportError, mapErrors
    , withDefault, merge
    )

{-| This module extends [`Data`](Remote-Data) preserving the information when reloading the same source.

It helps in scenarios with like this:

1.  Data was never requested
      - Start it with `Recyclable.NeverAsked`

2.  Request was sent
      - Pipe it into:

            |> Recyclable.toLoading

      - At this point, it will be:

            Recyclable.Fabricating Recyclable.Loading

3.  Request's `Response` was received (a `Failure error`)
      - Pipe it into:

            |> Recyclable.mergeResponse response

      - At this point, it will be:

            Recyclable.Fabricating (Recyclable.Failure error)

4.  User press "Retry" button, a new request was sent
      - Pipe it into:

            |> Recyclable.toLoading

      - At this point, it will be:

            Recyclable.Fabricating Recyclable.Loading

5.  Request's `Response` was received (a `Success data`)
      - Pipe it into:

            |> Recyclable.mergeResponse response

      - At this point, it will be:

            Recyclable.Ready data

6.  User press "Refresh" button, a new request was sent
      - Pipe it into:

            |> Recyclable.toLoading

      - At this point, it will be:

            Recyclable.Fabricating (Recyclable.Recycling data Recyclable.Loading)

7.  Request's `Response` was received (a `Failure error`)
      - Pipe it into:

            |> Recyclable.mergeResponse response

      - At this point, it will be:

            Recyclable.Ready (Recyclable.Recycling data (Recyclable.Failure error))

8.  User press "Refresh" button, a new request was sent
      - Pipe it into:

            |> Recyclable.toLoading

      - At this point, it will be:

            Recyclable.Fabricating (Recyclable.Recycling data Recyclable.Loading)

9.  Request's `Response` was received (a `Success data`)
      - Pipe it into:

            |> Recyclable.mergeResponse response

      - At this point, it will be:

            Recyclable.Ready data


# Types

@docs Recyclable, SubState, GraphqlHttpRecyclable


# Model

First, when initializing a "Model", you will use either:

  - `Recyclable.NeverAsked`

  - `(Recyclable.firstLoading, modelRequestCmd)`

@docs firstLoading


# Update

Then, on "update" you're gonna be using either:

@docs mergeResponse, toLoading, fromResponse


# Identity crisis

@docs isReady, isError, isCustomError, isTransportError, isLoading, isNeverAsked


# Common transformations

@docs getError
@docs map, mapCustomError, mapTransportError, mapErrors
@docs withDefault, merge

-}

import Graphql.Http as GraphqlHttp
import Remote.Errors exposing (RemoteError(..))
import Remote.Response as Response exposing (Response)


{-| A representation for fetchable-data with eight states:

First routine states:

  - `NotAsked`
      - We haven't asked for the data yet.
  - `Fabricating Loading`
      - We've asked, but haven't got an answer yet.
  - `Fabricating (Failure (Custom error))`
      - We asked, but we received one of the custom-defined errors instead.
        Here's the error.
  - `Fabricating (Failure (Transport error))`
      - We asked, but something went wrong on the network-side.
        Here's the error.
  - `Ready data`
      - Everything worked, and here's the data.

Future cycles states:

  - `Recycling data Loading`
      - We asked once more, and didn't got the new answer yet. Here's the previous data.
  - `Recycling data (Failure (Custom error))`
      - We asked once more, but the new answer was one of the custom-defined errors instead.
        Here's the previous data and the current error.
  - `Recycling data (Failure (Transport error))`
      - We asked once more, but the new request got lost on the network-side.
        Here's the previous data and the current error.

-}
type Recyclable transportError customError object
    = NeverAsked
    | Fabricating (SubState transportError customError)
    | Ready object
    | Recycling object (SubState transportError customError)


{-| Indicates `Failure` and `Loading` state when data is/was being fetched.
-}
type SubState transportError customError
    = Loading
    | Failure (RemoteError transportError customError)


{-| While [`Recyclable`](#Recyclable) can model any type of errors,
the most common one Paack has encountered is when fetching data from a Graphql query,
and get back [`GraphqlError`][GraphqlError].
Because of that, `GraphqlHttpRecyclable` is provided as a useful alias.

[GraphqlError]: /packages/dillonkearns/elm-graphql/latest/Graphql-Http-GraphqlError
[original]: /packages/krisajenkins/remotedata/latest/RemoteData#WebData

-}
type alias GraphqlHttpRecyclable error object =
    Recyclable (GraphqlHttp.RawError () GraphqlHttp.HttpError) error object


{-| It's very common to initialize the model already requesting the data,
use `firstLoading` in this case. Like this:

    init : RequestConfig -> ( Model, Cmd Msg )
    init requestConfig =
        ( Recyclable.firstLoading
        , modelRequestCmd requestConfig
        )

-}
firstLoading : Recyclable transportError customError object
firstLoading =
    Fabricating Loading


{-| This is the update routine for when overwriting the current
data with a new freshily-fetched response.

    update msg model =
        case msg of
            CardFetched response ->
                { model
                    | card =
                        Recyclable.mergeResponse response model.card
                }

-}
mergeResponse :
    Response transportError customError object
    -> Recyclable transportError customError object
    -> Recyclable transportError customError object
mergeResponse response data =
    case response of
        Response.Success object ->
            Ready object

        Response.Failure error ->
            case data of
                NeverAsked ->
                    Fabricating (Failure error)

                Fabricating _ ->
                    Fabricating (Failure error)

                Recycling object _ ->
                    Recycling object (Failure error)

                Ready object ->
                    Recycling object (Failure error)


{-| Convert a `Response`, probably produced from a query result, to a `Recyclable` value.

**NOTE**: As this function discards the previous information,
in most cases you should be using `mergeResponse` instead.

-}
fromResponse :
    Response transportError customError object
    -> Recyclable transportError customError object
fromResponse response =
    case response of
        Response.Failure error ->
            Fabricating (Failure error)

        Response.Success object ->
            Ready object


{-| If the recyclable is `Success` return the value,
but if the recyclable is anything else, then return a given default value.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
withDefault : object -> Recyclable transportError customError object -> object
withDefault default data =
    case data of
        Ready object ->
            object

        _ ->
            default


{-| Perfumery for doing pipes instead of switch-case
-}
merge : object -> Recyclable object object object -> object
merge default data =
    case data of
        NeverAsked ->
            default

        Fabricating Loading ->
            default

        Recycling _ Loading ->
            default

        Fabricating (Failure (Custom object)) ->
            object

        Fabricating (Failure (Transport object)) ->
            object

        Recycling _ (Failure (Custom object)) ->
            object

        Recycling _ (Failure (Transport object)) ->
            object

        Ready object ->
            object


{-| Returns a `Recyclable` to its loading state.
Keeping the information when available.
-}
toLoading : Recyclable transportError customError object -> Recyclable transportError customError object
toLoading data =
    case data of
        NeverAsked ->
            Fabricating Loading

        Fabricating _ ->
            Fabricating Loading

        Recycling object _ ->
            Recycling object Loading

        Ready object ->
            Recycling object Loading


{-| Apply a function to a positive (current or previous) value.
If the data doesn't contain a positive value, the same value will propagate through.
-}
map : (a -> b) -> Recyclable transportError customError a -> Recyclable transportError customError b
map applier data =
    case data of
        NeverAsked ->
            NeverAsked

        Fabricating subState ->
            Fabricating subState

        Recycling object subState ->
            Recycling (applier object) subState

        Ready object ->
            Ready (applier object)


{-| Transform a `Failure` value. If the data is `Failure`, it will be converted.
If the data is anything else, the same value will propagate through.
-}
mapErrors : (RemoteError transportError customError -> a) -> Recyclable transportError customError b -> Recyclable a a b
mapErrors applier data =
    case data of
        Recycling object (Failure (Custom error)) ->
            Recycling object (Failure (Custom (applier (Custom error))))

        Recycling object (Failure (Transport error)) ->
            Recycling object (Failure (Transport (applier (Transport error))))

        Fabricating (Failure (Custom error)) ->
            Fabricating (Failure (Custom (applier (Custom error))))

        Fabricating (Failure (Transport error)) ->
            Fabricating (Failure (Transport (applier (Transport error))))

        NeverAsked ->
            NeverAsked

        Recycling object Loading ->
            Recycling object Loading

        Fabricating Loading ->
            Fabricating Loading

        Ready object ->
            Ready object


{-| Transform a `Failure (Custom a)` value. If the data is `Failure (Custom a)`, it will be converted.
If the data is anything else, the same value will propagate through.
-}
mapCustomError : (customError -> a) -> Recyclable transportError customError object -> Recyclable transportError a object
mapCustomError applier response =
    case response of
        Recycling object (Failure (Custom error)) ->
            Recycling object (Failure (Custom (applier error)))

        Recycling object (Failure (Transport error)) ->
            Recycling object (Failure (Transport error))

        Fabricating (Failure (Custom error)) ->
            Fabricating (Failure (Custom (applier error)))

        Fabricating (Failure (Transport error)) ->
            Fabricating (Failure (Transport error))

        NeverAsked ->
            NeverAsked

        Recycling object Loading ->
            Recycling object Loading

        Fabricating Loading ->
            Fabricating Loading

        Ready object ->
            Ready object


{-| Transform a `Failure (Transport a)` value. If the data is `Failure (Transport a)`, it will be converted.
If the data is anything else, the same value will propagate through.
-}
mapTransportError : (transportError -> a) -> Recyclable transportError customError object -> Recyclable a customError object
mapTransportError applier response =
    case response of
        Recycling object (Failure (Custom error)) ->
            Recycling object (Failure (Custom error))

        Recycling object (Failure (Transport error)) ->
            Recycling object (Failure (Transport (applier error)))

        Fabricating (Failure (Custom error)) ->
            Fabricating (Failure (Custom error))

        Fabricating (Failure (Transport error)) ->
            Fabricating (Failure (Transport (applier error)))

        NeverAsked ->
            NeverAsked

        Recycling object Loading ->
            Recycling object Loading

        Fabricating Loading ->
            Fabricating Loading

        Ready object ->
            Ready object


{-| Transforms `Failure error` into `Just error`, and anything else into `Nothing`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
getError : Recyclable transportError customError object -> Maybe (RemoteError transportError customError)
getError data =
    case data of
        Recycling _ (Failure error) ->
            Just error

        Fabricating (Failure error) ->
            Just error

        _ ->
            Nothing


{-| `True` when `Ready _`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
isReady : Recyclable transportError customError object -> Bool
isReady data =
    case data of
        Ready _ ->
            True

        _ ->
            False


{-| `True` when `_ Loading`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
isLoading : Recyclable transportError customError object -> Bool
isLoading data =
    case data of
        Fabricating Loading ->
            True

        Recycling _ Loading ->
            True

        _ ->
            False


{-| `True` when `_ (Failure _)`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
isError : Recyclable transportError customError object -> Bool
isError data =
    case data of
        Fabricating (Failure _) ->
            True

        Recycling _ (Failure _) ->
            True

        _ ->
            False


{-| `True` when `_ (Failure (Transport _))`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
isTransportError : Recyclable transportError customError object -> Bool
isTransportError data =
    case data of
        Fabricating (Failure (Transport _)) ->
            True

        Recycling _ (Failure (Transport _)) ->
            True

        _ ->
            False


{-| `True` when `_ (Failure (Custom _))`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
isCustomError : Recyclable transportError customError object -> Bool
isCustomError data =
    case data of
        Fabricating (Failure (Custom _)) ->
            True

        Recycling _ (Failure (Custom _)) ->
            True

        _ ->
            False


{-| `True` when `NeverAsked`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
isNeverAsked : Recyclable transportError customError object -> Bool
isNeverAsked data =
    case data of
        NeverAsked ->
            True

        _ ->
            False
