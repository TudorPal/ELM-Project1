module Model exposing (..)

import Html exposing (b, div, p, text)
import Model.Date as Date
import Model.Event as Event exposing (Event)
import Model.Event.Category exposing (EventCategory(..), SelectedEventCategories, allSelected)
import Model.Interval as Interval
import Model.PersonalDetails exposing (DetailWithName, PersonalDetails)
import Model.Repo exposing (Repo)


type alias Model =
    { personalDetails : PersonalDetails
    , events : List Event
    , selectedEventCategories : SelectedEventCategories
    , repos : List Repo
    }


academicEvents : List Event
academicEvents =
    [ { title = "High School Degree"
      , interval = Interval.withDurationYears (Date.onlyYear 2016) 4
      , description = p [] [ text "I obtained ", b [] [ text "very" ], text " good grades." ]
      , category = Academic
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Student at the University of Cluj-Napoca"
      , interval = Interval.open (Date.full 2020 Date.Sep)
      , description = div [] []
      , category = Academic
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


workEvents : List Event
workEvents =
    [ { title = "Work event 1"
      , interval = Interval.withDurationMonths 2020 Date.Oct 9
      , description = text "IT Teacher at my previous high school"
      , category = Work
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Work event 2"
      , interval = Interval.open (Date.full 2022 Date.Jun)
      , description = text "Product Data Analyst at my current company"
      , category = Work
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


projectEvens : List Event
projectEvens =
    [ { title = "Personal project 1"
      , interval = Interval.oneYear 2019
      , description = text "Small website using html and css"
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Personal project 2"
      , interval = Interval.withDurationMonths 2022 Date.Mar 2
      , description = text "16 bit Microprocessor"
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    , { title = "Personal project 3"
      , interval = Interval.oneYear 2022
      , description = text "Elm website"
      , category = Project
      , url = Nothing
      , tags = []
      , important = False
      }
    ]


personalDetails : PersonalDetails
personalDetails =
    { name = "Pal Tudor"
    , intro = "Product Data Analyst and Computer Engineering student"
    , contacts = [ DetailWithName "email" "pal.tudor10@gmail.com" ]
    , socials = [ DetailWithName "github" "https://github.com/tudorpal" ]
    }


initModel : Model
initModel =
    { personalDetails = personalDetails
    , events = Event.sortByInterval <| academicEvents ++ workEvents ++ projectEvens
    , selectedEventCategories = allSelected
    , repos = []
    }
