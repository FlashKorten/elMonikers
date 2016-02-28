module ElMonikers (main) where

import Html exposing (Html)
import Signal exposing (Mailbox, Signal, mailbox, merge, foldp)
import Time exposing (every, second)
import Random exposing (initialSeed)

import ElMonikers.Model exposing (Action(NoOp,Tick), Model, initialModel)
import ElMonikers.View exposing (view)
import ElMonikers.Update exposing (update)

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
