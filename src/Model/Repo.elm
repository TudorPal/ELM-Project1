module Model.Repo exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Json.Decode as De


type alias Repo =
    { name : String
    , description : Maybe String
    , url : String
    , pushedAt : String
    , stars : Int
    }


view : Repo -> Html msg
view repo =
    div [ class "repo" ]
        [ h2 [class "repo-name"] [ text repo.name ]
        , p [class "repo-description"] [ text <| Maybe.withDefault "" repo.description ]
        , p [class "repo-url"] [ a [ href repo.url ] [ text repo.url ] ]
        , p [] [ text <| "Pushed at: " ++ repo.pushedAt ]
        , p [class "repo-stars"] [ text <| String.fromInt repo.stars ++ " stars" ]
        ]
    -- Debug.todo "Implement Model.Repo.view"


sortByStars : List Repo -> List Repo
sortByStars repos =
    List.sortBy .stars repos
    -- Debug.todo "Implement Model.Repo.sortByStars"


{-| Deserializes a JSON object to a `Repo`.
Field mapping (JSON -> Elm):

  - name -> name
  - description -> description
  - html\_url -> url
  - pushed\_at -> pushedAt
  - stargazers\_count -> stars

-}

-- Model - laboratorul 7
decodeRepo : De.Decoder Repo
decodeRepo =
    De.map5 Repo
        (De.field "name" De.string)
        (De.field "description" (De.maybe De.string))
        (De.field "html_url" De.string)
        (De.field "pushed_at" De.string)
        (De.field "stargazers_count" De.int)
