module ElMonikers.View (view) where

import Html exposing (Html, Attribute, text, h1, h2, div, input, button, ul, li, hr)
import Html.Attributes exposing (class, id, autofocus, name, value, placeholder, type')
import Html.Events exposing (onClick, on, targetValue)
import Signal exposing (Address, Signal)
import List exposing (map, reverse, sortBy, indexedMap)
import ElMonikers.Model exposing (Action(..), State(..), Zip, Card, Model, Team, activeTeamName)
import ElMonikers.I18n exposing (Iso(..), lookup, queryCodes)
import Dict exposing (Dict)

view : Address Action -> Model -> Html
view address model =
  div
    [ id "container" ]
    [ pageHeader address model.dict
    , stateView address model
    ]

localeChooser : Address Action -> List Html
localeChooser address = let b iso = button [class ("locale" ++ toString iso), onClick address (SetLocale iso)] [ text (toString iso)]
                        in map b queryCodes

pageHeader : Address Action -> Dict String String -> Html
pageHeader address d = div [ class "head" ]
                           [ div [ class "locale" ] (localeChooser address)
                           , h1 [] [ text <| lookup d "Title" ]
                           ]

inPlayButtons : Address Action -> Dict String String -> Html
inPlayButtons address d =
  div
    []
    [ div []
        [ button [ class "solved", onClick address Solved ] [ text <| lookup d "Solved" ]
        , button [ class "later", onClick address NotSolved ] [ text <| lookup d "Unsolved" ]
        ]
    ]

statsView : List Team -> Html
statsView teams =
  case teams of
    p :: _ -> div [ class "teams" ]
                  [ h2 [ class "name" ]
                       [ text (p.name ++ " - " ++ toString p.score) ]]
    []     -> div [ class "unreachableBranch" ]
                  [ text "if you see me, there is a major logic error happening..."]

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


playViewBottom : Address Action -> Dict String String -> Html
playViewBottom address d =
  div [] [ inPlayButtons address d]

playViewCard : Address Action -> Zip Card -> Html
playViewCard address ( _, _, c, _ ) =
  case c of
    []     -> div [class "unreachableBranch"] [ text "if you see me, there is a major logic error happening..."]
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
    , playViewBottom address model.dict
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
    , text ((lookup model.dict "Timer") ++ toString model.maxCount)
    , button [class "minus", onClick address DecTimer] [ text "-5" ]
    ]

onInput : Address Action -> (String -> Action) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

startView : Address Action -> Model -> Html
startView address model = let indexedPlayers = indexedMap (,) model.teams
  in div [] [ timerInput address model
            , ul     [class "teams"]
                     (map (mkTeamInput address) indexedPlayers)
            , button [class "addTeam", onClick address AddTeam]
                     [ text <| lookup model.dict "AddTeam" ]
            , button [class "fullPositive", onClick address NextRound]
                     [ text <| infoTextForRound model]
            ]

endView : Address Action -> Model -> Html
endView address model =
  div
    [ id "end" ]
    [ ul [] (map scoreListEntry <| reverse <| sortBy .score model.teams)
    , button [ class "next", onClick address NextRound ] [ text <| infoTextForRound model ]
    , button [ class "restart", onClick address Init ] [ text <| lookup model.dict "Restart" ]
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
        [ text ((lookup model.dict "NextTeam") ++ (activeTeamName model)) ]
    ]

stateView : Address Action -> Model -> Html
stateView address model =
  case model.state of
    Play   -> playView   address model
    Start  -> startView  address model
    Switch -> switchView address model
    End    -> endView    address model
