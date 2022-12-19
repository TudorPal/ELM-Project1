module Model.Event exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href)
import Model.Event.Category exposing (EventCategory(..))
import Model.Interval as Interval exposing (Interval)


type alias Event =
    { title : String
    , interval : Interval
    , description : Html Never
    , category : EventCategory
    , url : Maybe String
    , tags : List String
    , important : Bool
    }


categoryView : EventCategory -> Html Never
categoryView category =
    case category of
        Academic ->
            text "Academic"

        Work ->
            text "Work"

        Project ->
            text "Project"

        Award ->
            text "Award"


sortByInterval : List Event -> List Event
sortByInterval events =
    let
        compareEvents event1 event2 =
            Interval.compare event1.interval event2.interval
    in 
        List.sortWith compareEvents events

    -- Debug.todo "Implement Event.sortByInterval"

--Functie pentru a verifica daca evenimentul este important
eventImportant : Bool -> String
eventImportant important1 =
    if important1 == True then
        "event event-important"
    else
        "event"

view : Event -> Html Never
view event =
    div [class (eventImportant event.important)] 
    [
        h1 [class "event-title"] [text event.title]
        , div [class "event-interval"] [Interval.view event.interval]
        , p [class "event-description"][event.description]
        , p [class "event-category"] [categoryView event.category]
        , a [class "event-url", href (Maybe.withDefault "#" event.url) ] [ text (Maybe.withDefault "#" event.url) ]

    ]
    -- Debug.todo "Implement the Model.Event.view function"
