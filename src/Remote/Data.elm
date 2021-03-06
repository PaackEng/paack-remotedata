module Remote.Data exposing
    ( RemoteData(..), GraphqlHttpData
    , fromResponse
    , isSuccess, isError, isCustomError, isTransportError, isLoading, isNotAsked
    , toError
    , map, mapCustomError, mapTransportError, mapErrors
    , withDefault, merge
    )

{-| A datatype representing fetched data in five different states.
Based on [Kris's `RemoteData`][original].

For the same motivations that are presented in Kris' version,
with the small addition of always using [transport and custom errors](Remote-Errors),
and aliasing `GraphqlError` instead of `Http.Error`.

[original]: /packages/krisajenkins/remotedata/latest/RemoteData


# Types

@docs RemoteData, GraphqlHttpData


# Update

@docs fromResponse


# Identity crisis

@docs isSuccess, isError, isCustomError, isTransportError, isLoading, isNotAsked


# Common transformations

@docs toError
@docs map, mapCustomError, mapTransportError, mapErrors
@docs withDefault, merge

-}

import Graphql.Http as GraphqlHttp
import Remote.Errors exposing (RemoteError(..))
import Remote.Response as Response exposing (Response)


{-| Frequently when you're fetching data from an API, you want to represent five different states:

  - `NotAsked` - We haven't asked for the data yet.
  - `Loading` - We've asked, but haven't got an answer yet.
  - `Failure (Custom error)` - We asked, but we received one of the custom-defined errors instead. Here's the error.
  - `Failure (Transport error)` - We asked, but something went wrong on the network-side. Here's the error.
  - `Success data` - Everything worked, and here's the data.

Based on [Kris's `RemoteData`][original].

[original]: /packages/krisajenkins/remotedata/latest/RemoteData#RemoteData

-}
type RemoteData transportError customError object
    = NotAsked
    | Loading
    | Failure (RemoteError transportError customError)
    | Success object


{-| While [`RemoteData`](#RemoteData) can model any type of errors,
the most common one Paack has encountered is when fetching data from a Graphql query,
and getting back a [`GraphqlError`][GraphqlError].
Because of that, `GraphqlHttpData` is provided as a useful alias.

Based on [Kris's `WebData`][original].

[GraphqlError]: /packages/dillonkearns/elm-graphql/latest/Graphql-Http-GraphqlError
[original]: /packages/krisajenkins/remotedata/latest/RemoteData#WebData

-}
type alias GraphqlHttpData error object =
    RemoteData (GraphqlHttp.RawError () GraphqlHttp.HttpError) error object


{-| Convert a `Response`, probably produced from a query result, to a `RemoteData` value.

Based on [Kris's `fromResult`][original].

[original]: /packages/krisajenkins/remotedata/latest/RemoteData#fromResult

-}
fromResponse :
    Response transportError customError object
    -> RemoteData transportError customError object
fromResponse response =
    case response of
        Response.Failure error ->
            Failure error

        Response.Success object ->
            Success object


{-| If the data is `Success` return its value,
but if the data is anything else then return a given default alternative.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
withDefault : object -> RemoteData transportError customError object -> object
withDefault default data =
    case data of
        Success object ->
            object

        _ ->
            default


{-| For doing pipes instead of switch-case.

    someData
        |> RemoteData.map (always "Success")
        |> RemoteData.mapCustomError (always "Expected failure")
        |> RemoteData.mapTransportError (always "Network error")
        |> RemoteData.merge "Loading or never asked"

-}
merge : object -> RemoteData object object object -> object
merge default data =
    case data of
        NotAsked ->
            default

        Loading ->
            default

        Failure (Custom object) ->
            object

        Failure (Transport object) ->
            object

        Success object ->
            object


{-| Apply a function to a `RemoteData.Success`. If the data is `Success`, it will be converted.
If the data is anything else, the same value will propagate through.
-}
map : (a -> b) -> RemoteData transportError customError a -> RemoteData transportError customError b
map applier data =
    case data of
        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Failure error ->
            Failure error

        Success object ->
            Success (applier object)


{-| Transform a `Failure` value. If the data is `Failure`, it will be converted.
If the data is anything else, the same value will propagate through.
-}
mapErrors : (RemoteError transportError customError -> a) -> RemoteData transportError customError b -> RemoteData a a b
mapErrors applier data =
    case data of
        Failure (Custom error) ->
            Failure (Custom (applier (Custom error)))

        Failure (Transport error) ->
            Failure (Transport (applier (Transport error)))

        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Success object ->
            Success object


{-| Transform a `Failure (Custom a)` value. If the data is `Failure (Custom a)`, it will be converted.
If the data is anything else, the same value will propagate through.
-}
mapCustomError : (customError -> a) -> RemoteData transportError customError object -> RemoteData transportError a object
mapCustomError applier response =
    case response of
        Failure (Custom error) ->
            Failure (Custom (applier error))

        Failure (Transport error) ->
            Failure (Transport error)

        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Success object ->
            Success object


{-| Transform a `Failure (Transport a)` value. If the data is `Failure (Transport a)`, it will be converted.
If the data is anything else, the same value will propagate through.
-}
mapTransportError : (transportError -> a) -> RemoteData transportError customError object -> RemoteData a customError object
mapTransportError applier response =
    case response of
        Failure (Custom error) ->
            Failure (Custom error)

        Failure (Transport error) ->
            Failure (Transport (applier error))

        NotAsked ->
            NotAsked

        Loading ->
            Loading

        Success object ->
            Success object


{-| Transforms `Failure error` into `Just error`, and anything else into `Nothing`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
toError : RemoteData transportError customError object -> Maybe (RemoteError transportError customError)
toError data =
    case data of
        Failure error ->
            Just error

        Loading ->
            Nothing

        NotAsked ->
            Nothing

        Success _ ->
            Nothing


{-| `True` when `Success _`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isSuccess : RemoteData transportError customError object -> Bool
isSuccess data =
    case data of
        Success _ ->
            True

        _ ->
            False


{-| `True` when `Loading _`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isLoading : RemoteData transportError customError object -> Bool
isLoading data =
    case data of
        Loading ->
            True

        _ ->
            False


{-| `True` when `Failure _`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isError : RemoteData transportError customError object -> Bool
isError data =
    case data of
        Failure _ ->
            True

        _ ->
            False


{-| `True` when `Failure (Transport _)`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isTransportError : RemoteData transportError customError object -> Bool
isTransportError data =
    case data of
        Failure (Transport _) ->
            True

        _ ->
            False


{-| `True` when `Failure (isCustomError _)`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isCustomError : RemoteData transportError customError object -> Bool
isCustomError data =
    case data of
        Failure (Custom _) ->
            True

        _ ->
            False


{-| `True` when `NotAsked`.

**NOTE**: This function opposes the purpose of this package by eliminating not aimed states. Always evaluate using a switch-case instead.

-}
isNotAsked : RemoteData transportError customError object -> Bool
isNotAsked data =
    case data of
        NotAsked ->
            True

        _ ->
            False
