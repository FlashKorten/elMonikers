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
                   , ("Round1",    "1st Round: Do whatever you like.")
                   , ("Round2",    "2nd Round: Only a single word allowed.")
                   , ("Round3",    "3rd Round: Only gestures allowed.")
                   , ("Round4",    "4th Round: Facial expressions only.")
                   , ("Round4+",   "Next Round: Use your own rules.")
                   ]
    De -> fromList [ ("Title",     "elMonikers!")
                   , ("NextTeam",  "Als Nächstes: ")
                   , ("Solved",    "Korrekt")
                   , ("Unsolved",  "Später...")
                   , ("AddTeam",   "+ Team")
                   , ("Restart",   "Neustart")
                   , ("Error",     "Fehler... gar nicht gut.")
                   , ("Timer",     "Timer: ")
                   , ("Round1",    "Runde 1: Beschreib wie du magst.")
                   , ("Round2",    "Runde 2: Nur 1 Wort erlaubt.")
                   , ("Round3",    "Runde 3: Nur Gesten erlaubt.")
                   , ("Round4",    "Runde 4: Nur Mimik erlaubt.")
                   , ("Round4+",    "Nächste Runde: Lass' dir was einfallen.")
                   ]

lookup : Dict String String -> String -> String
lookup d l = withDefault "Translation missing..." <| get l d
