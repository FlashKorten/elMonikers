module ElMonikers.View (..) where

import Html exposing (Html, Attribute, text, h1, h2, div, input, button, ul, li, hr)
import Html.Attributes exposing (class, id, autofocus, name, value, placeholder, type')
import Html.Events exposing (onClick, on, targetValue)
import Signal exposing (Address, Signal)
import List exposing (map, reverse, sortBy, indexedMap)
import ElMonikers.Model exposing (..)

view : Address Action -> Model -> Html
view address model =
  div
    [ id "container" ]
    [ pageHeader
    , stateView address model
    ]

pageHeader : Html
pageHeader =
  h1 [] [ text "elMonikers!" ]

inPlayButtons : Address Action -> Html
inPlayButtons address =
  div
    []
    [ div []
        [ button [ class "solved", onClick address Solved ] [ text "Solved" ]
        , button [ class "later", onClick address NotSolved ] [ text "Later..." ]
        ]
    ]

statsView : List Team -> Html
statsView teams =
  case teams of
    p :: _ -> div [ class "teams" ] [ h2 [ class "name" ] [ text (p.name ++ " - " ++ toString p.score) ] ]
    []     -> div [ class "error" ] [ text "Get some teams" ]

playViewTop : Model -> Html
playViewTop model =
  div
    []
    [ h1
        [ if model.count < 4 then
            class "counterAlert"
          else
            class "counter"
        ]
        [ text (toString model.count) ]
    , statsView model.teams
    ]


playViewBottom : Address Action -> Html
playViewBottom address =
  div [] [ inPlayButtons address ]

playViewCard : Address Action -> Zip Card -> Html
playViewCard address ( _, _, c, _ ) =
  case c of
    []     -> div [] [ text "Empty!!!" ]
    x :: _ -> div
                []
                [ hr [] []
                , div [] [ h2 [ class "title" ] [ text x.title ] ]
                , hr [] []
                , div [ class "desc" ] [ text x.description ]
                , hr [] []
                , div [ class "classification" ] [ text (x.genre ++ " / " ++ x.category) ]
                ]

playView : Address Action -> Model -> Html
playView address model =
  div
    []
    [ playViewTop model
    , playViewCard address model.cards
    , playViewBottom address
    ]

scoreListEntry : Team -> Html
scoreListEntry p =
  li [class "highscore"] [ text (p.name ++ ": " ++ toString p.score) ]

mkTeamInput : Address Action -> (Int, Team) -> Html
mkTeamInput address (i, t) =
  li [] [ input [ type' "text"
                , class "teamName"
                , placeholder t.name
                , value t.name
                , name ("team" ++ toString i)
                , autofocus False
                , onInput address (UpdateName i)
                ] []
        , button [class "removeTeam", onClick address (RemoveTeam i)] [ text "X" ]
        ]

timerInput : Address Action -> Model -> Html
timerInput address model =
  div [class "timer"]
    [ button [class "plus", onClick address IncTimer] [ text "+5" ]
    , text ("Timer: " ++ toString model.maxCount ++ " seconds")
    , button [class "minus", onClick address DecTimer] [ text "-5" ]
    ]

onInput : Address Action -> (String -> Action) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

startView : Address Action -> Model -> Html
startView address model = let indexedPlayers = indexedMap (,) model.teams
  in div [] [ timerInput address model
            , ul     [class "teams"]                              (map (mkTeamInput address) indexedPlayers)
            , button [class "addTeam",      onClick address AddTeam] [ text "Add Team" ]
            , button [class "fullPositive", onClick address NextRound] [ text <| infoTextForRound model]
            ]

endView : Address Action -> Model -> Html
endView address model =
  div
    [ id "end" ]
    [ ul [] (map scoreListEntry <| reverse <| sortBy .score model.teams)
    , button [ class "next", onClick address NextRound ] [ text <| infoTextForRound model ]
    , button [ class "restart", onClick address Init ] [ text "Restart" ]
    ]

infoTextForRound : Model -> String
infoTextForRound model = case model.round of
    1 -> "1st Round: Do whatever you like."
    2 -> "2nd Round: Only a single word allowed."
    3 -> "3rd Round: Only gestures allowed."
    4 -> "4th Round: Facial expressions only."
    _ -> "Next Round: Use your own rules."

switchView : Address Action -> Model -> Html
switchView address model =
  div
    [ id "switch" ]
    [ button
        [ class "fullPositive", onClick address NextTeam ]
        [ text ("Next up: " ++ (activeTeamName model)) ]
    ]

stateView : Address Action -> Model -> Html
stateView address model =
  case model.state of
    Play   -> playView   address model
    Start  -> startView  address model
    Switch -> switchView address model
    End    -> endView    address model
