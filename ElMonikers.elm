module ElMonikers (..) where

import Html exposing (Html)
import Signal exposing (Mailbox, Signal, mailbox, merge, foldp)
import List exposing (take, drop)
import Time exposing (every, second)
import Random exposing (initialSeed)
import ElMonikers.Model exposing (..)
import ElMonikers.View exposing (..)

update : Action -> Model -> Model
update action model =
  case action of
    NoOp    -> model
    Init    -> let m = initialModel model.nextSeed
                in { m | teams = resetTeamScores model.teams
                       , maxCount = model.maxCount
                       , count = model.count}
    NotSolved   -> { model | cards = nextElem model.cards }
    NextTeam -> { model | state = Play }
    Solved  -> case model.cards of
        ( 1, _, _, _ ) -> { model | teams = incScore model.teams
                                  , cards = removeElem model.cards
                                  , round = model.round + 1
                                  , state = End}
        _              -> { model | cards = removeElem model.cards
                                  , teams = incScore model.teams }
    NextRound -> let (shuffledCards, seed) = reshuffleDiscards model
                  in {model | state = Play
                            , cards = shuffledCards
                            , count = if model.round == 1 then model.maxCount else model.count
                            , nextSeed = seed}
    -- Configuration
    IncTimer -> {model | maxCount = model.maxCount + 5}
    DecTimer -> {model | maxCount = if model.maxCount > 5 then model.maxCount - 5 else model.maxCount}
    AddTeam -> {model | teams = model.teams ++ [ newTeam "New Team" ]}
    RemoveTeam n -> {model | teams = (take n model.teams) ++ (drop (n + 1) model.teams)}
    UpdateName n name -> {model | teams = (updateName n name model.teams)}
    -- Timer Event
    Tick    -> case model.state of
                 Play -> case model.count of
                           0 -> { model | teams = cycle model.teams
                                        , cards = nextElem model.cards
                                        , state = Switch
                                        , count = model.maxCount }
                           n -> { model | count = n - 1 }
                 _    -> model

-- PATCHING

port startTime : Float

ticks : Signal Action
ticks = Signal.map (\_ -> Tick) (every second)

actionsMB : Mailbox Action
actionsMB = mailbox NoOp

clicks : Signal Action
clicks = actionsMB.signal

actions : Signal Action
actions = merge clicks ticks

model : Signal Model
model = foldp update (startTime |> round |> initialSeed |> initialModel) actions

main : Signal Html
main = Signal.map (view actionsMB.address) model
