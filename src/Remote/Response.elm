module Remote.Response exposing
    ( Response(..), GraphqlHttpResponse
    , fromResults, graphqlHttpToMsg
    , isSuccess, isFailure, isCustomError, isTransportError
    , toMaybe, getError
    , map, mapCustomError, mapTransportError, mapErrors
    , withDefault, merge
    )

{-| This module exists for harmonizing queries results with the rest of this package.

Without it, the type of these results would need to be something like `Response transportError (Response customError object)`.


# Types

@docs Response, GraphqlHttpResponse


# Creation

@docs fromResults, graphqlHttpToMsg


# Identity crisis

@docs isSuccess, isFailure, isCustomError, isTransportError


# Common transformations

@docs toMaybe, getError


# Generic transformations

@docs map, mapCustomError, mapTransportError, mapErrors
@docs withDefault, merge

-}

import Graphql.Http as GraphqlHttp
import Remote.Errors exposing (RemoteError(..))
import Result exposing (Result)


{-| A `Remote.Result` is either `Success` meaning the query succeeded,
or it is an `Failure` meaning that there was some failure.

A `Failure` is then sub-divided accordingly to [`Remote.Errors`](Remote-Errors).

-}
type Response transportError customError object
    = Failure (RemoteError transportError customError)
    | Success object


{-| While [`Response`](#Response) can model any type of errors,
the most common one Paack has encountered is when fetching data from a Graphql query,
and get back [`GraphqlError`][GraphqlError].
Because of that, `GraphqlHttpResponse` is provided as a useful alias.

[GraphqlError]: /packages/dillonkearns/elm-graphql/latest/Graphql-Http-GraphqlError
[original]: /packages/krisajenkins/remotedata/latest/RemoteData#RemoteData

-}
type alias GraphqlHttpResponse customError object =
    Response (GraphqlHttp.RawError () GraphqlHttp.HttpError) customError object


{-| `True` when `Success _`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
isSuccess : Response transportError customError object -> Bool
isSuccess response =
    case response of
        Success _ ->
            True

        _ ->
            False


{-| `True` when `Failure _`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
isFailure : Response transportError customError object -> Bool
isFailure response =
    case response of
        Success _ ->
            False

        _ ->
            True


{-| `True` when `Failure (Custom _)`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
isCustomError : Response transportError customError object -> Bool
isCustomError response =
    case response of
        Failure (Custom _) ->
            True

        _ ->
            False


{-| `True` when `Failure (Transport _)`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
isTransportError : Response transportError customError object -> Bool
isTransportError response =
    case response of
        Failure (Transport _) ->
            True

        _ ->
            False


{-| Tranforms `Response transportError (Response customError object)`,
received from a query result, into a `Response transportError customError object`.

**NOTE**: Prefer [`graphqlHttpToMsg`](#graphqlHttpToMsg) and get `Response` directly in the resulting message.

-}
fromResults :
    Result transportError (Result customError object)
    -> Response transportError customError object
fromResults results =
    case results of
        Result.Err transportError ->
            Failure (Transport transportError)

        Result.Ok (Result.Err customError) ->
            Failure (Custom customError)

        Result.Ok (Result.Ok object) ->
            Success object


{-| Convert to a simpler `Maybe` if the actual error message is not needed or
you need to interact with some code that primarily uses maybes.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
toMaybe : Response transportError customError object -> Maybe object
toMaybe response =
    case response of
        Failure _ ->
            Nothing

        Success object ->
            Just object


{-| Transforms `Failure error` into `Just error`, and `Success _` into `Nothing`.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
getError : Response transportError customError object -> Maybe (RemoteError transportError customError)
getError response =
    case response of
        Failure error ->
            Just error

        Success _ ->
            Nothing


{-| Prepares a message that will return `GraphqlHttpResponse` for Graphql usage.

    query
        |> Graphql.Http.queryRequest "https://..."
        |> Graphql.Http.send
            (Response.graphqlHttpToMsg Msg.BookingFetched)

-}
graphqlHttpToMsg :
    (GraphqlHttpResponse customError object -> msg)
    -> Result (GraphqlHttp.Error (Result customError object)) (Result customError object)
    -> msg
graphqlHttpToMsg message input =
    input
        |> GraphqlHttp.discardParsedErrorData
        |> fromResults
        |> message


{-| Apply a function to a posive result. If the result is `Success`, it will be converted.
If the result is a `Failure`, the same error value will propagate through.
-}
map : (a -> b) -> Response transportError customError a -> Response transportError customError b
map applier response =
    case response of
        Failure error ->
            Failure error

        Success object ->
            Success (applier object)


{-| Transform a `Failure` value. If the result is `Failure`, it will be converted.
If the result is a `Success`, the same value will propagate through.
-}
mapErrors : (RemoteError transportError customError -> a) -> Response transportError customError b -> Response a a b
mapErrors applier response =
    case response of
        Failure (Custom error) ->
            Failure (Custom (applier (Custom error)))

        Failure (Transport error) ->
            Failure (Transport (applier (Transport error)))

        Success object ->
            Success object


{-| Transform a `Failure (Custom a)` value. If the result is `Failure (Custom a)`, it will be converted.
If the result is a `Success _` or `Failure (Transport _)`, the same value will propagate through.
-}
mapCustomError : (customError -> a) -> Response transportError customError object -> Response transportError a object
mapCustomError applier response =
    case response of
        Failure (Custom customError) ->
            Failure (Custom (applier customError))

        Success object ->
            Success object

        Failure (Transport transportError) ->
            Failure (Transport transportError)


{-| Transform a `Failure (Transport a)` value. If the result is `Failure (Transport a)`, it will be converted.
If the result is a `Success _` or `Failure (Custom _)`, the same value will propagate through.
-}
mapTransportError : (transportError -> a) -> Response transportError customError object -> Response a customError object
mapTransportError applier response =
    case response of
        Failure (Custom customError) ->
            Failure (Custom customError)

        Success object ->
            Success object

        Failure (Transport transportError) ->
            Failure (Transport (applier transportError))


{-| If the result is `Success` return the value,
but if the result is a `Failure` then return a given default value.

**NOTE**: This function implicates in information-loss. Prefer using a switch-case, or use it very wisely.

-}
withDefault : object -> Response transportError customError object -> object
withDefault default response =
    case response of
        Failure _ ->
            default

        Success object ->
            object


{-| Perfumery for doing pipes instead of switch-case.

    someResponse
        |> Response.map (always "Success")
        |> Response.mapCustomError (always "Expected failure")
        |> Response.mapTransportError (always "Network error")
        |> Response.merge

-}
merge : Response object object object -> object
merge response =
    case response of
        Failure (Custom object) ->
            object

        Success object ->
            object

        Failure (Transport object) ->
            object
