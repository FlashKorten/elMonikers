module ElMonikers (..) where

import Html exposing (Html)
import Signal exposing (Mailbox, Signal, mailbox, merge, foldp)
import List exposing (take, drop)
import Time exposing (every, second)
import Random exposing (initialSeed)
import ElMonikers.Model exposing (..)
import ElMonikers.View exposing (..)

updateInit : Model -> Model
updateInit m = let model = initialModel m.nextSeed
               in { model | teams = resetTeamScores m.teams
                  , maxCount = m.maxCount
                  , count = m.count}

updateSolved : Model -> Model
updateSolved m = case m.cards of
        ( 1, _, _, _ ) -> { m | teams = incScore m.teams
                              , cards = removeElem m.cards
                              , round = m.round + 1
                              , state = End}
        _              -> { m | teams = incScore m.teams
                              , cards = removeElem m.cards }

updateNotSolved : Model -> Model
updateNotSolved m = { m | cards = nextElem m.cards }

updateNextTeam : Model -> Model
updateNextTeam m = { m | state = Play }

updateNextRound : Model -> Model
updateNextRound m = let (shuffledCards, seed) = reshuffleDiscards m
                     in { m | state = Play
                            , cards = shuffledCards
                            , count = if m.round == 1 then m.maxCount else m.count
                            , nextSeed = seed}

updateTick : Model -> Model
updateTick m = case m.state of
                 Play -> case m.count of
                           0 -> { m | teams = cycle m.teams
                                    , cards = nextElem m.cards
                                    , state = Switch
                                    , count = m.maxCount }
                           n -> { m | count = n - 1 }
                 _    -> m

updateIncTimer : Model -> Model
updateIncTimer m = { m| maxCount = m.maxCount + 5}

updateDecTimer : Model -> Model
updateDecTimer m = { m | maxCount = if m.maxCount > 5 then m.maxCount - 5 else m.maxCount}

updateAddTeam : Model -> Model
updateAddTeam m = { m | teams = m.teams ++ [ newTeam "New Team" ]}

updateRemoveTeam : Model -> Int -> Model
updateRemoveTeam m n = { m | teams = (take n m.teams) ++ (drop (n + 1) m.teams)}

updateUpdateName : Model -> Int -> String -> Model
updateUpdateName m n name = { m | teams = (updateName n name m.teams)}

update : Action -> Model -> Model
update action model =
  case action of
    NoOp              -> model
    Init              -> updateInit model
    Solved            -> updateSolved model
    NotSolved         -> updateNotSolved model
    NextTeam          -> updateNextTeam model
    NextRound         -> updateNextRound model
    -- Configuration
    IncTimer          -> updateIncTimer model
    DecTimer          -> updateDecTimer model
    AddTeam           -> updateAddTeam model
    RemoveTeam n      -> updateRemoveTeam model n
    UpdateName n name -> updateUpdateName model n name
    -- Timer Event
    Tick              -> updateTick model

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
