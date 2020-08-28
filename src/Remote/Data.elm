module Remote.Data exposing
    ( RemoteData(..), GraphqlHttpData
    , fromResponse
    , isSuccess, isError, isCustomError, isTransportError, isLoading, isNeverAsked
    , getError
    , map, mapCustomError, mapTransportError, mapErrors
    , withDefault, merge
    )

{-|


# Types

@docs RemoteData, SubState, GraphqlHttpData


# Update

@docs fromResponse


# Identity crisis

@docs isSuccess, isError, isCustomError, isTransportError, isLoading, isNeverAsked


# Common transformations

@docs getError


# Generic transformations

@docs map, mapCustomError, mapTransportError, mapErrors
@docs withDefault, merge

-}

import Graphql.Http as GraphqlHttp
import Remote.Errors exposing (RemoteError(..))
import Remote.Response as Response exposing (Response)


type RemoteData transportError customError object
    = NotAsked
    | Loading
    | Failure (RemoteError transportError customError)
    | Success object


type alias GraphqlHttpData error object =
    RemoteData (GraphqlHttp.RawError () GraphqlHttp.HttpError) error object


fromResponse :
    Response transportError customError object
    -> RemoteData transportError customError object
fromResponse response =
    case response of
        Response.Failure error ->
            Failure error

        Response.Success object ->
            Success object


withDefault : object -> RemoteData transportError customError object -> object
withDefault default data =
    case data of
        Success object ->
            object

        _ ->
            default


{-| Perfumary for doing pipes instead of switch-case
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


getError : RemoteData transportError customError object -> Maybe (RemoteError transportError customError)
getError data =
    case data of
        Failure error ->
            Just error

        Loading ->
            Nothing

        NotAsked ->
            Nothing

        Success _ ->
            Nothing


isSuccess : RemoteData transportError customError object -> Bool
isSuccess data =
    case data of
        Success _ ->
            True

        _ ->
            False


isLoading : RemoteData transportError customError object -> Bool
isLoading data =
    case data of
        Loading ->
            True

        _ ->
            False


isError : RemoteData transportError customError object -> Bool
isError data =
    case data of
        Failure _ ->
            True

        _ ->
            False


isTransportError : RemoteData transportError customError object -> Bool
isTransportError data =
    case data of
        Failure (Transport _) ->
            True

        _ ->
            False


isCustomError : RemoteData transportError customError object -> Bool
isCustomError data =
    case data of
        Failure (Custom _) ->
            True

        _ ->
            False


isNeverAsked : RemoteData transportError customError object -> Bool
isNeverAsked data =
    case data of
        NotAsked ->
            True

        _ ->
            False
