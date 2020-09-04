module Remote.Recyclable exposing
    ( Recyclable(..), GraphqlHttpRecyclable
    , firstLoading
    , mergeResponse, toLoading, fromResponse
    , isReady, isError, isCustomError, isTransportError, isLoading, isNeverAsked
    , toError
    , map, mapCustomError, mapTransportError, mapErrors
    , withDefault, merge
    , RecyclingStage(..)
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

@docs toError
@docs map, mapCustomError, mapTransportError, mapErrors
@docs withDefault, merge

-}

import Graphql.Http as GraphqlHttp
import Remote.Errors exposing (RemoteError(..))
import Remote.Response as Response exposing (Response)


{-| A representation for fetchable data with eight states:

First routine states:

  - `NotAsked`
      - We haven't asked for the data yet.
  - `Loading`
      - We've asked, but haven't got an answer yet.
  - `Failure (Custom error)`
      - We asked, but we received one of the custom-defined errors instead.
        Here's the error.
  - `Failure (Transport error)`
      - We asked, but something went wrong on the network-side.
        Here's the error.
  - `Ready data`
      - Everything worked, and here's the data.

Future cycles states:

  - `Recycling data LoadingStage`
      - We asked once more, and didn't got the new answer yet. Here's the previous data.
  - `Recycling data (FailureStage (Custom error))`
      - We asked once more, but the new answer was one of the custom-defined errors instead.
        Here's the previous data and the current error.
  - `Recycling data (FailureStage (Transport error))`
      - We asked once more, but the new request got lost on the network-side.
        Here's the previous data and the current error.

-}
type Recyclable transportError customError object
    = NeverAsked
    | Loading
    | Failure (RemoteError transportError customError)
    | Ready object
    | Recycling object (RecyclingStage transportError customError)


{-| Indicates `FailureStage` and `LoadingStage` stage when data is/was being fetched.
-}
type RecyclingStage transportError customError
    = LoadingStage
    | FailureStage (RemoteError transportError customError)


{-| While [`Recyclable`](#Recyclable) can model any type of errors,
the most common one Paack has encountered is when fetching data from a Graphql query,
and getting back a [`GraphqlError`][GraphqlError].
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
    Loading


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
                    Failure error

                Failure _ ->
                    Failure error

                Loading ->
                    Failure error

                Recycling object _ ->
                    Recycling object (FailureStage error)

                Ready object ->
                    Recycling object (FailureStage error)


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
            Failure error

        Response.Success object ->
            Ready object


{-| If the recyclable is `Success` return the value,
but if the recyclable is anything else, then return a given default value.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
withDefault : object -> Recyclable transportError customError object -> object
withDefault default data =
    case data of
        Ready object ->
            object

        _ ->
            default


{-| For doing pipes instead of switch-case
-}
merge : object -> Recyclable object object object -> object
merge default data =
    case data of
        NeverAsked ->
            default

        Loading ->
            default

        Recycling _ LoadingStage ->
            default

        Failure (Custom object) ->
            object

        Failure (Transport object) ->
            object

        Recycling _ (FailureStage (Custom object)) ->
            object

        Recycling _ (FailureStage (Transport object)) ->
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
            Loading

        Loading ->
            Loading

        Failure _ ->
            Loading

        Recycling object _ ->
            Recycling object LoadingStage

        Ready object ->
            Recycling object LoadingStage


{-| Apply a function to the values in `Recycling value _` and `Ready value`.
If the data is anything else, the same value will propagate through.
-}
map : (a -> b) -> Recyclable transportError customError a -> Recyclable transportError customError b
map applier data =
    case data of
        NeverAsked ->
            NeverAsked

        Loading ->
            Loading

        Failure error ->
            Failure error

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
        Recycling object (FailureStage (Custom error)) ->
            Recycling object (FailureStage (Custom (applier (Custom error))))

        Recycling object (FailureStage (Transport error)) ->
            Recycling object (FailureStage (Transport (applier (Transport error))))

        Failure (Custom error) ->
            Failure (Custom (applier (Custom error)))

        Failure (Transport error) ->
            Failure (Transport (applier (Transport error)))

        NeverAsked ->
            NeverAsked

        Recycling object LoadingStage ->
            Recycling object LoadingStage

        Loading ->
            Loading

        Ready object ->
            Ready object


{-| Transform a `Failure (Custom a)` value. If the data is `Failure (Custom a)`, it will be converted.
If the data is anything else, the same value will propagate through.
-}
mapCustomError : (customError -> a) -> Recyclable transportError customError object -> Recyclable transportError a object
mapCustomError applier response =
    case response of
        Recycling object (FailureStage (Custom error)) ->
            Recycling object (FailureStage (Custom (applier error)))

        Recycling object (FailureStage (Transport error)) ->
            Recycling object (FailureStage (Transport error))

        Failure (Custom error) ->
            Failure (Custom (applier error))

        Failure (Transport error) ->
            Failure (Transport error)

        NeverAsked ->
            NeverAsked

        Recycling object LoadingStage ->
            Recycling object LoadingStage

        Loading ->
            Loading

        Ready object ->
            Ready object


{-| Transform a `Failure (Transport a)` value. If the data is `Failure (Transport a)`, it will be converted.
If the data is anything else, the same value will propagate through.
-}
mapTransportError : (transportError -> a) -> Recyclable transportError customError object -> Recyclable a customError object
mapTransportError applier response =
    case response of
        Recycling object (FailureStage (Custom error)) ->
            Recycling object (FailureStage (Custom error))

        Recycling object (FailureStage (Transport error)) ->
            Recycling object (FailureStage (Transport (applier error)))

        Failure (Custom error) ->
            Failure (Custom error)

        Failure (Transport error) ->
            Failure (Transport (applier error))

        NeverAsked ->
            NeverAsked

        Recycling object LoadingStage ->
            Recycling object LoadingStage

        Loading ->
            Loading

        Ready object ->
            Ready object


{-| Transforms `Failure error` into `Just error`, and anything else into `Nothing`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
toError : Recyclable transportError customError object -> Maybe (RemoteError transportError customError)
toError data =
    case data of
        Recycling _ (FailureStage error) ->
            Just error

        Failure error ->
            Just error

        _ ->
            Nothing


{-| `True` when `Ready _`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isReady : Recyclable transportError customError object -> Bool
isReady data =
    case data of
        Ready _ ->
            True

        _ ->
            False


{-| `True` when `_ Loading`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isLoading : Recyclable transportError customError object -> Bool
isLoading data =
    case data of
        Loading ->
            True

        Recycling _ LoadingStage ->
            True

        _ ->
            False


{-| `True` when `_ (Failure _)`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isError : Recyclable transportError customError object -> Bool
isError data =
    case data of
        Failure _ ->
            True

        Recycling _ (FailureStage _) ->
            True

        _ ->
            False


{-| `True` when `_ (Failure (Transport _))`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isTransportError : Recyclable transportError customError object -> Bool
isTransportError data =
    case data of
        Failure (Transport _) ->
            True

        Recycling _ (FailureStage (Transport _)) ->
            True

        _ ->
            False


{-| `True` when `_ (Failure (Custom _))`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isCustomError : Recyclable transportError customError object -> Bool
isCustomError data =
    case data of
        Failure (Custom _) ->
            True

        Recycling _ (FailureStage (Custom _)) ->
            True

        _ ->
            False


{-| `True` when `NeverAsked`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isNeverAsked : Recyclable transportError customError object -> Bool
isNeverAsked data =
    case data of
        NeverAsked ->
            True

        _ ->
            False
