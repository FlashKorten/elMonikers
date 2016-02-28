module ElMonikers.I18n (Iso(..), initDict, lookup, queryCodes) where

import Dict exposing (..)
import Maybe exposing (withDefault)

type Iso = En | De

queryCodes : List Iso
queryCodes = [En, De]

initDict : Iso -> Dict String String
initDict iso = case iso of
    En -> fromList [ ("Title",     "elMonikers!")
                   , ("NextTeam",  "Next: ")
                   , ("Solved",    "Solved")
                   , ("Unsolved",  "Later...")
                   , ("AddTeam",   "Add Team")
                   , ("Restart",   "Restart")
                   , ("Error",     "Oops... this is uncool.")
                   , ("Timer",     "Timer: ")
                   ]
    De -> fromList [ ("Title",     "elMonikers!")
                   , ("NextTeam",  "Als Nächstes: ")
                   , ("Solved",    "Korrekt")
                   , ("Unsolved",  "Später...")
                   , ("AddTeam",   "+ Team")
                   , ("Restart",   "Neustart")
                   , ("Error",     "Fehler... gar nicht gut.")
                   , ("Timer",     "Timer: ")
                   ]

lookup : Dict String String -> String -> String
lookup d l = withDefault "Translation missing..." <| get l d
