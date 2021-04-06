module Remote.Errors exposing (RemoteError(..), GraphqlHttpError)

{-|

@docs RemoteError, GraphqlHttpError

-}

import Graphql.Http as GraphqlHttp


{-| Abstract union for merging transport errors with custom decoder errors.
-}
type RemoteError transportError customError
    = Custom customError
    | Transport transportError


{-| Implementation of [`RemoteError`](#RemoteError) for failures containing [`Graphql.Http.GraphqlError`][GraphqlError]

[GraphqlError]: /packages/dillonkearns/elm-graphql/latest/Graphql-Http-GraphqlError

-}
type alias GraphqlHttpError customError =
    RemoteError (GraphqlHttp.RawError () GraphqlHttp.HttpError) customError
