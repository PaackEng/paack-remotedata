module Remote.Errors exposing (RemoteError(..), GraphqlHttpError)

{-|

@docs RemoteError, GraphqlHttpError

-}

import Graphql.Http as GraphqlHttp


type RemoteError transportError customError
    = Custom customError
    | Transport transportError


type alias GraphqlHttpError customError =
    RemoteError (GraphqlHttp.RawError () GraphqlHttp.HttpError) customError
