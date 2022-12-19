module Model.PersonalDetails exposing (..)

import Html exposing (..)
import Html.Attributes exposing (href)


type alias DetailWithName =
    { name : String
    , detail : String
    }


type alias PersonalDetails =
    { name : String
    , contacts : List DetailWithName
    , intro : String
    , socials : List DetailWithName
    }

view : PersonalDetails -> Html msg
view details =
    div [] 
        [ 
        h1 [Html.Attributes.id "name"] [text details.name]
        , em [Html.Attributes.id "intro"] [text details.intro]
        , ul [] (List.map viewContact details.contacts)
        , ul [] (List.map viewSocial details.socials)
        ]
    -- Debug.todo "Implement the Model.PersonalDetails.view function"

-- Functie pentru a putea adauga fiecarui contact clasa "contact-detail"
viewContact : DetailWithName -> Html msg
viewContact contact =
    li [Html.Attributes.class "contact-detail"] [text contact.name, text ": ", text contact.detail]

-- Functie pentru a putea adauga fiecarui link clasa "social-link"
viewSocial : DetailWithName -> Html msg
viewSocial social =
    li [Html.Attributes.class "social-link"] 
    [
     text social.name, text ": ", a [href social.detail][text social.detail]
    ]