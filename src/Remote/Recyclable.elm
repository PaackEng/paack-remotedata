module Remote.Recyclable exposing
    ( Recyclable(..), SubState(..), GraphqlHttpRecyclable
    , firstLoading
    , mergeResponse, fromResponse
    , isReady, isError, isCustomError, isTransportError, isLoading, isNeverAsked
    , toLoading
    , map, mapCustomError, mapTransportError, mapErrors
    , withDefault, merge
    , getError
    )

{-| This module extends [`Data`](Remote-Data) preserving the information when reloading the same source.

It helps in scenarios with like this:

1.  Data was never requested
      - `= Recyclable.NeverAsked`
2.  Request was sent
      - `|> Recyclable.toLoading`
      - `== Recyclable.Fabricating Recyclable.Loading`
3.  Request's `Response` was received (a `Failure _`)
      - `|> Recyclable.mergeResponse response`
      - `== Recyclable.Fabricating (Recyclable.Failure _)`
4.  User press "Retry", a new request was sent
      - `|> Recyclable.toLoading`
      - `== Recyclable.Fabricating Recyclable.Loading`
5.  Request's `Response` was received (a `Success data`)
      - `|> Recyclable.mergeResponse response`
      - `== Recyclable.Ready data`
6.  User press "Refresh", a new request was sent
      - `|> Recyclable.toLoading`
      - `== Recyclable.Fabricating (Recyclable.Recycling data Recyclable.Loading)`
7.  Request's `Response` was received (a `Failure _`)
      - `|> Recyclable.mergeResponse response`
      - `== Recyclable.Ready (Recyclable.Recycling data (Recyclable.Failure _))`
8.  User press "Refresh", a new request was sent
      - `|> Recyclable.toLoading`
      - `== Recyclable.Fabricating (Recyclable.Recycling data Recyclable.Loading)`
9.  Request's `Response` was received (a `Success`)
      - `|> Recyclable.mergeResponse response`
      - `== Recyclable.Ready _`


# Types

@docs Recyclable, SubState, GraphqlHttpRecyclable


# Model

When initializing a "Model", you will use either:

    - `Recyclable.NeverAsked`

    - `(Recyclable.firstLoading, requestCmd)`

@docs firstLoading


# Update

On "update" you're gonna be using either:

@docs mergeResponse, fromResponse


# Identity crisis

@docs isReady, isError, isCustomError, isTransportError, isLoading, isNeverAsked


# State rollback

@docs toLoading


# Generic transformations

@docs map, mapCustomError, mapTransportError, mapErrors
@docs withDefault, merge

-}

import Graphql.Http as GraphqlHttp
import Remote.Errors exposing (RemoteError(..))
import Remote.Response as Response exposing (Response)


{-| A `Recyclable` is

First routine:

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

Future cycles:

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


type SubState transportError customError
    = Loading
    | Failure (RemoteError transportError customError)


type alias GraphqlHttpRecyclable error object =
    Recyclable (GraphqlHttp.RawError () GraphqlHttp.HttpError) error object


firstLoading : Recyclable transportError customError object
firstLoading =
    Fabricating Loading


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


{-| **NOTE**: As this function discards the previous information, in most cases you shold be using `mergeResponse` instead.
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


withDefault : object -> Recyclable transportError customError object -> object
withDefault default data =
    case data of
        Ready object ->
            object

        _ ->
            default


{-| Perfumary for doing pipes instead of switch-case
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


getError : Recyclable transportError customError object -> Maybe (RemoteError transportError customError)
getError data =
    case data of
        Recycling _ (Failure error) ->
            Just error

        Fabricating (Failure error) ->
            Just error

        Fabricating _ ->
            Nothing

        Recycling _ _ ->
            Nothing

        NeverAsked ->
            Nothing

        Ready _ ->
            Nothing


isReady : Recyclable transportError customError object -> Bool
isReady data =
    case data of
        Ready _ ->
            True

        _ ->
            False


isLoading : Recyclable transportError customError object -> Bool
isLoading data =
    case data of
        Fabricating Loading ->
            True

        Recycling _ Loading ->
            True

        _ ->
            False


isError : Recyclable transportError customError object -> Bool
isError data =
    case data of
        Fabricating (Failure _) ->
            True

        Recycling _ (Failure _) ->
            True

        _ ->
            False


isTransportError : Recyclable transportError customError object -> Bool
isTransportError data =
    case data of
        Fabricating (Failure (Transport _)) ->
            True

        Recycling _ (Failure (Transport _)) ->
            True

        _ ->
            False


isCustomError : Recyclable transportError customError object -> Bool
isCustomError data =
    case data of
        Fabricating (Failure (Custom _)) ->
            True

        Recycling _ (Failure (Custom _)) ->
            True

        _ ->
            False


isNeverAsked : Recyclable transportError customError object -> Bool
isNeverAsked data =
    case data of
        NeverAsked ->
            True

        _ ->
            False
