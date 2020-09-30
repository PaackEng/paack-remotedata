module Remote.Dict.Recyclable exposing
    ( update, mergeResponse, toLoading
    , get
    )

{-| This module is composed of helper functions for handling [`Recyclables`](Remote-Recyclable) inside [Elm's `Dict`][elm-dict].


# Building

@docs update, mergeResponse, toLoading


# Query

@docs get

[elm-dict]: /packages/elm/core/latest/Dict

-}

import Dict exposing (Dict)
import Remote.Recyclable as Recyclable exposing (Recyclable)
import Remote.Response exposing (Response)


{-| Reduce the result of a `Dict.get` operation by transforming `Nothing` in `NeverAsked`.
-}
get :
    comparable
    -> Dict comparable (Recyclable a b c)
    -> Recyclable a b c
get key dict =
    dict
        |> Dict.get key
        |> Maybe.withDefault Recyclable.NeverAsked


{-| Similar to [`Dict.update`][dict-update], but using `NeverAsked` for not-found keys and also for dropping updated values.
-}
update :
    comparable
    -> (Recyclable a b c -> Recyclable a b c)
    -> Dict comparable (Recyclable a b c)
    -> Dict comparable (Recyclable a b c)
update key applier =
    Dict.update key
        (\old ->
            case
                old
                    |> Maybe.withDefault
                        Recyclable.NeverAsked
                    |> applier
            of
                Recyclable.NeverAsked ->
                    Nothing

                other ->
                    Just other
        )


{-| Shortcut for updating one value back to its loading state.
-}
toLoading :
    comparable
    -> Dict comparable (Recyclable a b c)
    -> Dict comparable (Recyclable a b c)
toLoading key =
    update key Recyclable.toLoading


{-| Shortcut for merging a new freshily-fetched response to the current state.
-}
mergeResponse :
    comparable
    -> Response a b c
    -> Dict comparable (Recyclable a b c)
    -> Dict comparable (Recyclable a b c)
mergeResponse key response =
    Dict.update key
        (Maybe.map (Recyclable.mergeResponse response)
            >> Maybe.withDefault (Recyclable.fromResponse response)
            >> Just
        )
