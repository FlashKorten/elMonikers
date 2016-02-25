module ElMonikers where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Signal exposing (Address)
import List exposing (map,reverse,length,sortBy)

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

statsView : List Player -> Html
statsView players = case players of
  (p::_) -> div [ class "player" ] [ div [ class "name"] [text p.name]
                         , div [ class "score"] [text (toString p.score)]]
  []     -> div [ class "error"] [text "Get some players"]

playViewBottom : Address Action -> Html
playViewBottom address = div [] [inPlayButtons address]

playViewCard : Address Action -> Zip Card -> Html
playViewCard address (a, b, c) = case c of
      []     -> div [] [text "Empty!!!"]
      (x::_) -> div [] [text x.title
                       -- , p [] [text (toString a)]
                       -- , p [] [text (toString b)]
                       -- , p [] [text (toString c)]
                       ]

playView : Address Action -> Model -> Html
playView address model = div [] [ statsView model.players
                                , playViewCard address model.cards
                                , playViewBottom address]

scoreListEntry : Player -> Html
scoreListEntry p = li [] [text (p.name ++ ": " ++ toString p.score)]

endView : Address Action -> Model -> Html
endView address model = div [ id "end"] [
                              ul [] (map scoreListEntry <| reverse <| sortBy .score model.players),
                              button [onClick address Init][text "Start Game"]]

switchView : Address Action -> Model -> Html
switchView address model = div [ id "switch"]
                               [ statsView model.players
                               , button [onClick address Unpause]
                                        [text "Go on..."]
                               ]

stateView : Address Action -> Model -> Html
stateView address model = case model.state of
  Play -> playView address model
  Start -> div [] [text "Start"]
  Switch -> switchView address model
  End -> endView address model

pageHeader : Html
pageHeader =
  h1 [ ] [ text "elMonikers!" ]


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
            | Init

update : Action -> Model -> Model
update action model = case action of
  Init   -> initialModel
  NoOp   -> model
  Later  -> {model | cards = nextElem model.cards}
  Next   -> {model | players = cycle model.players
                   , state = Switch}
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

