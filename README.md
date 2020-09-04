# Paack's RemoteData for Elm

A different approach to [Kris Jenkins' RemoteData](https://github.com/krisajenkins/remotedata).


## Differences

* RemoteData.Failure is always sub-divided into transport errors and custom errors;
* An extra module "Recyclable" for reusing information while fresh data is loading;
* There are shorthand types for data loaded using Graphql.


## Why?

Using Kris' RemoteData, our team realized that we needed to support both the transport errors (e.g., Graphql errors, HTTP errors) and any other custom error sent by our API (e.g., invalid credentials and invalid input).
That way, our models were ending up with types like `RemoteData TransportErrorsPlusCustomErrors SomeData`.

With that in mind, and to reduce code repetition, we decided to merge both failures within RemoteData.

Alongside that, we came up with a structure for when some information is reloading, and we do not loose the previously loaded one.
We called it Recyclable.

Following our use-case scenarios, we changed the main star from `Http.Error` to `Graphql.Http.HttpError` instead.
However, support for a different transport protocol is achievable using more abstract types.


## Installation

From your top-level directory - the one with `elm.json` in - call:

```
$ elm install PaackEng/paack-remotedata
```


## Documentation

See [package's page](http://package.elm-lang.org/packages/PaackEng/paack-remotedata/latest) for the complete documentation.


## Usage

Here's an example of how to use this package:

```elm
-- Graphql queries, data types, and errors unions.
import Api.Author as Author exposing (Author)
import Api.Authors as Authors exposing (AuthorBrief)
-- Elm-UI
import Element exposing (Element)
import Element.Input as Input
-- This package
import Remote.Data as RemoteData exposing (GraphqlHttpData)
import Remote.Errors as RemoteErrors
import Remote.Recyclable as Recyclable exposing (GraphqlHttpRecyclable)
import Remote.Response as Response exposing (GraphqlHttpResponse)


type Msg
    = AuthorFetched (GraphqlHttpResponse Author.Error Author)
    | AuthorSelect Author.Id
    | AuthorsFetched (GraphqlHttpResponse Authors.Error (List AuthorBrief))
    | Reload


type alias Model =
    { authors : GraphqlHttpData Authors.Error (List AuthorBrief)
    , selectedAuthor : GraphqlHttpRecyclable Author.Error Author
    }


init : RequestConfig -> ( Model, Cmd Msg )
init requestConfig =
    ( { authors = RemoteData.Loading
      , selectedAuthor = Recyclable.NotAsked
      }
    , Authors.request requestConfig AuthorsFetched
    )


update : RequestConfig -> Model -> Model -> ( Model, Cmd Msg )
update requestConfig model msg =
    case msg of
        AuthorFetched response ->
            ( { model | authors = RemoteData.fromResponse response }
            , Cmd.none
            )

        AuthorSelect id ->
            ( { model
                | selectedAuthor =
                    Recyclable.toLoading model.selectedAuthor
              }
            , Author.request requestConfig id AuthorFetched
            )

        AuthorsFetched response ->
            ( { model
                | selectedAuthor =
                    Recyclable.mergeResponse
                        response
                        model.selectedAuthor
              }
            , Cmd.none
            )

        Reload ->
            init requestConfig


view : Model -> Element Msg
view model =
    Element.column [ Element.width fill ]
        [ pickAuthorView model
        , selectedAuthorView model
        ]


pickAuthorView : Model -> Element Msg
pickAuthorView model =
    case model.authors of
        RemoteData.Success [] ->
            Element.text "There is no author available."

        RemoteData.Success list ->
            List.map
                (\{ id, name } ->
                    Input.button []
                        { onPress = AuthorSelect id, label = name }
                )
                list

        RemoteData.Loading ->
            Element.text "Loading authors list..."

        RemoteData.NotAsked ->
            Element.button []
                { onPress = Reload, label = "Reload page" }

        RemoteData.Failure (RemoteErrors.Custom Authors.InvalidCredentials) ->
            Element.text "You must login to access the author list!"

        RemoteData.Failure (RemoteErrors.Transport _) ->
            Element.text "Connection lost!"


selectedAuthorView : Model -> Element Msg
selectedAuthorView model =
    case model.selectedAuthor of
        Recyclable.NeverAsked ->
            Element.text "Please, pick an author!"

        Recyclable.Loading ->
            Element.text "Loading"

        Recyclable.Failure _ ->
            Element.text "Failed to load author!"

        Recyclable.Recycling { name } newState ->
            Element.row
                [ case newState of
                    Recyclable.Loading ->
                        Element.tet "Loading new author..."

                    Recyclable.Failure _ ->
                        Element.tet "Unable to load new author!"
                , Element.text ("You previously picked: " ++ name)
                ]

        Recyclable.Ready { name } ->
            Element.text ("You picked: " ++ name)
```


## License

Copyright © 2020 Paack 

Distributed under the MIT license.
