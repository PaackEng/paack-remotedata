module Remote.Response exposing
    ( Response(..), GraphqlHttpResponse
    , fromResults, graphqlHttpToMsg
    , isSuccess, isFailure, isCustomError, isTransportError
    , toMaybe, getError
    , map, mapCustomError, mapTransportError, mapErrors
    , withDefault, merge
    )

{-|


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


type Response transportError customError object
    = Failure (RemoteError transportError customError)
    | Success object


type alias GraphqlHttpResponse customError object =
    Response (GraphqlHttp.RawError () GraphqlHttp.HttpError) customError object


isSuccess : Response transportError customError object -> Bool
isSuccess response =
    case response of
        Success _ ->
            True

        _ ->
            False


isFailure : Response transportError customError object -> Bool
isFailure response =
    case response of
        Success _ ->
            False

        _ ->
            True


isCustomError : Response transportError customError object -> Bool
isCustomError response =
    case response of
        Failure (Custom _) ->
            True

        _ ->
            False


isTransportError : Response transportError customError object -> Bool
isTransportError response =
    case response of
        Failure (Transport _) ->
            True

        _ ->
            False


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


toMaybe : Response transportError customError object -> Maybe object
toMaybe response =
    case response of
        Failure _ ->
            Nothing

        Success object ->
            Just object


getError : Response transportError customError object -> Maybe (RemoteError transportError customError)
getError response =
    case response of
        Failure error ->
            Just error

        Success _ ->
            Nothing


graphqlHttpToMsg :
    (GraphqlHttpResponse customError object -> msg)
    -> Result (GraphqlHttp.Error (Result customError object)) (Result customError object)
    -> msg
graphqlHttpToMsg message input =
    input
        |> GraphqlHttp.discardParsedErrorData
        |> fromResults
        |> message


map : (a -> b) -> Response transportError customError a -> Response transportError customError b
map applier response =
    case response of
        Failure error ->
            Failure error

        Success object ->
            Success (applier object)


mapErrors : (RemoteError transportError customError -> a) -> Response transportError customError b -> Response a a b
mapErrors applier response =
    case response of
        Failure (Custom error) ->
            Failure (Custom (applier (Custom error)))

        Failure (Transport error) ->
            Failure (Transport (applier (Transport error)))

        Success object ->
            Success object


mapCustomError : (customError -> a) -> Response transportError customError object -> Response transportError a object
mapCustomError applier response =
    case response of
        Failure (Custom customError) ->
            Failure (Custom (applier customError))

        Success object ->
            Success object

        Failure (Transport transportError) ->
            Failure (Transport transportError)


mapTransportError : (transportError -> a) -> Response transportError customError object -> Response a customError object
mapTransportError applier response =
    case response of
        Failure (Custom customError) ->
            Failure (Custom customError)

        Success object ->
            Success object

        Failure (Transport transportError) ->
            Failure (Transport (applier transportError))


withDefault : object -> Response transportError customError object -> object
withDefault default response =
    case response of
        Failure _ ->
            default

        Success object ->
            object


{-| Perfumary for doing pipes instead of switch-case
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
