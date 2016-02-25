module Monika where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import List exposing (map,reverse,length)

import StartApp.Simple as StartApp

-- MODEL

type State = Start | Switch | Play | End

type alias Zip a = (Int , List a, List a)

removeElem : Zip a -> Zip a
removeElem (n, past, future) =
  case future of
    [] -> (n-1, [], reverse past)
    (next::rest) -> case rest of
                      [] -> (n-1, [], reverse past)
                      x  -> (n-1, past, x)

nextElem : Zip a -> Zip a
nextElem (n, past, future) =
  case future of
    [] -> (n, [], reverse past)
    (current::rest) -> case rest of
                        [] -> (n, [], reverse (current::past))
                        x  -> (n, current::past, x)

cycle : List a -> List a
cycle l = case l of
  []      -> []
  (x::xs) -> reverse (x::(reverse xs))

type alias Player =
  { name : String
  , score : Int
  }

newPlayer : String -> Player
newPlayer name = { name = name
                 , score = 0
               }

type alias Card =
  { title : String
  }

newCard : String -> Card
newCard t = {title = t}

toCards : List String -> Zip Card
toCards l = (length l, [], map newCard l)

dummyCards : Zip Card
dummyCards = toCards ["Eins","Zwei","Drei","Vier"]

type alias Model =
  { players : List Player
  , cards : Zip Card
  , state : State
  }

dummyPlayers : List Player
dummyPlayers = [newPlayer "Player 1", newPlayer "Player 2"]

initialModel : Model
initialModel = { players = dummyPlayers
               , cards = dummyCards
               , state = Play
               }

-- VIEW

view : Address Action -> Model -> Html
view address model =
  div
    [ id "container" ]
    [ pageHeader,
      statsView model.players,
      stateView address model
    ]

inPlayButtons : Address Action -> Html
inPlayButtons address =
  div
    [ id "buttons" ]
    [ button
      [ class "solved", onClick address Solved ]
      [ text "Solved" ],
      button
      [ class "later", onClick address Later ]
      [ text "Later" ],
      button
      [ onClick address Next ]
      [ text "Next Player" ]
    ]

playView : Address Action -> Zip Card -> Html
playView address (a, b, c) = case c of
      []     -> div [] [text "Empty!!!"]
      (x::_) -> div [] [text x.title, p [][text (toString a)], p [][text (toString b)], p [][text (toString c)], buttons address]

stateView : Address Action -> Model -> Html
stateView address model = case model.state of
  Play -> playView address model.cards
  Start -> div [] [text "Start"]
  Switch -> div [ id "switch"] [button [onClick address Unpause][text "Start"]]
  End -> div [] [text "End"]

pageHeader : Html
pageHeader =
  h1 [ ] [ text "Monika!" ]

statsView : List Player -> Html
statsView players = case players of
  (p::_) -> div [ class "player" ] [ div [ class "name"] [text p.name]
                         , div [ class "score"] [text (toString p.score)]]
  []     -> div [ class "error"] [text "Get some players"]


-- UPDATE

incScore : List Player -> List Player
incScore players = case players of
                     (p::ps) -> {p | score = p.score + 1}::ps
                     []      -> players

type Action = NoOp
            | Solved
            | Later
            | Next
            | Unpause

update : Action -> Model -> Model
update action model = case action of
  NoOp   -> model
  Later  -> {model | cards = nextElem model.cards}
  Next   -> {model | players = cycle model.players
                   , state = Switch
            }
  Unpause -> {model | state = Play}
  Solved -> case model.cards of
    (1,_,_) -> {model | players = incScore model.players
                      , state = End}
    _       -> {model | cards = removeElem model.cards
                   , players = incScore model.players }

main: Signal Html
main =
  StartApp.start
    { model = initialModel,
      view  = view,
     update = update
    }

