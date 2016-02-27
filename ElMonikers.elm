module ElMonikers (..) where

import Html exposing (Html, Attribute, text, h1, h2, div, input, button, ul, li, hr)
import Html.Attributes exposing (class, id, autofocus, name, value, placeholder, type')
import Html.Events exposing (onClick, on, targetValue)
import Signal exposing (Mailbox, Address, Signal, mailbox, merge, foldp)
import List exposing (take, drop, map, reverse, length, sortBy, indexedMap)
import Time exposing (every, second)
import Random exposing (Seed, initialSeed)
import Shuffle exposing (rShuffle)

-- MODEL

type State
  = Start
  | Switch
  | Play
  | End

type alias Zip a = ( Int, List a, List a, List a )

removeElem : Zip a -> Zip a
removeElem ( n, past, future, done ) =
  case future of
    [] -> ( 42, [], [], [])
    next :: rest -> case rest of
        [] -> ( n - 1, [], reverse past, next::done )
        x  -> ( n - 1, past, x, next::done )

nextElem : Zip a -> Zip a
nextElem ( n, past, future, done ) =
  case future of
    [] -> ( n, [], reverse past, done )
    current :: rest -> case rest of
                         [] -> ( n, [], reverse (current :: past), done )
                         x  -> ( n, current :: past, x, done )

reshuffle : Model -> (Zip Card, Seed)
reshuffle model = case model.cards of
    (_, _, _, discards) -> let (l, s) = Shuffle.rShuffle 3 (discards, model.nextSeed)
                        in ((length l, [], l, []), s)

cycle : List a -> List a
cycle l = case l of
    []      -> []
    x :: xs -> reverse (x :: (reverse xs))

type alias Team =
  { name : String
  , score : Int
  }

newTeam : String -> Team
newTeam name =
  { name = name
  , score = 0
  }

type alias Card =
  { title : String
  , description : String
  , category : String
  , genre : String
  }

newCard : ( String, String, String, String ) -> Card
newCard ( g, c, t, d ) =
  { genre = g
  , category = c
  , title = t
  , description = d
  }

toCards : List ( String, String, String, String ) -> Zip Card
toCards l =
  ( length l, [], map newCard l, [] )

initialCards : List (String, String, String, String)
initialCards =
    [
      ( "Historical", "Scientist", "Heisenberg", "A German theoretical physicist, creator of the uncertainty principle, and winner of a Nobel Prize in Physics for his development of quantum mechanics. His name was also used as an alias for the meth manufacturer Walter White in the series Breaking Bad." )
    , ( "Fictional", "Simpsons", "Comic Book Guy", "The overweight owner of The Android's Dungeon & Baseball Card Shop on The Simpsons. His character has a master's degree in foldlore mythology - he translated The Lord of the Rings into Klingon - and is known for his catchphrase ''Worst [blank] ever.''" )
    , ( "Fictional", "AI", "Hal-9000", "The sentient computer from Stanley Kubrick's 2001: A Space Odyssey. Initially a helpful part of the ship, it eventually turns on the crew, killing Dr. Poole before being disconnected by Dave. As its mind goes, it sings ''Daisy Bell'' while pleading for its life." )
    , ( "Fictional", "Hero", "The Dude", "The nickname of Jeffrey Lebowski in the Cone Brothers fild The Big Lebowski. Played by Jeff Bridges, he is depicted as an unemployed slacker, pacifist, and bowler who drinks White Russians and listens to Creedence. He was based on the Seattle Seven member Jeff Dowd." )
    , ( "Historical", "Musician", "Kate Bush", "An English musician known for her idiosyncratic vocal delivery, literary sensibility, hit singles such as ''Wuthering Heights'' and ''Babooshka.'' One of her Karate instructors has noted that many of her dance moves owe a debt to her martial arts training." )
    , ( "Historical", "Director", "Werner Herzog", "A German directory, whose films often feature his own philosophical narration (''I believe the common denominator of the universe is not harmony, but chaos, hostility, and murder.'') He was once shot with an air rifle during an interview and continued as if nothing happened." )
    , ( "Historical", "Actor", "David Hasselhoff", "An actor and musician who starred in Knight Rider and Baywatch. His daughter once recorded a video of him, shirtless and drunk, trying to eat a hamburger on the floor of a Las Vegas hotel room, while she lectured him on the importance of sobriety. Germans love him." )
    , ( "Historical", "Actor", "Nicolas Cage", "An actor known for his range, prolific work ethic, and self described ''Nouveau Shamanic'' acting style. Appearing in such films as Moonstruck, Adaptation, and Bad Lieutenant: Port of Call New Orleans, he is known for his manic, non-naturalistic performances." )
    , ( "Fictional", "Monster", "The Alien from Alien", "The extraterrestrial from a famous action-horror film series. Sometimes referred to as a Xenomorph, these creatures are organized around a single queen, which gives birth to creatures that follow a lifecycle from egg to facehugger to chestburster to adult." )
    , ( "Mythological", "Hero", "Achilles", "An Ancient Greek hero and demigod from Homer's Iliad, who defeated the Trojan warrior Hector as revenge for killing his friend and lover Patroclus. He was later killed by an arrow to his heel - the only part of his body that was vulnerable to physical injury." )
    , ( "Historical", "Politician", "Shirtless Vladimir Putin", "Former KGB officer and current President of Russia. Under his rule, Russia has grown increasingly undemocratic. He cultivates a rugged image in state media, being shown riding half-dressed on horseback and ''discovering'' two Ancient Greek urns in the Black Sea." )
    , ( "Historical", "Actor", "Christopher Walken", "An actor known for his odd line readings, dancing skills, and creepily handsome face. Despite being an Academy Award winning actor, he is perhaps best known for the ''Weapon of Choice'' music video and as Bruce Dickinson, an SNL character who asks for more cowbell." )
    , ( "Fantasy", "Minor Character", "Hodor", "Winterfell's stableboy in George R.R. Martin's A Song of Ice and Fire and the Game of Thrones TV adaptation. He carries the crippled Bran Stark around in a large basket. Though his real name is Walder, he is only referred to by this name, since it is the only word he ever uses." )
    , ( "Historical", "Civil Rights", "Rosa Parks", "A civil rights activist who refused to give up her seat in the colored section of a bus to a white passenger. Claudette Colvin had done the same nine months earlier, but NAACP leadership did not want a pregnant, unmarried teenager as the face of the movement." )
    , ( "Fictional", "Monster", "Godzilla", "The King of the Monsters, who first appeared in a series of Japanese films made in response to the atomic bombings of Hiroshima and Nagasaki. The monster typically has a reptilian look, walks on two legs, has a long tail, and has ''nuclear breath.''" )
    , ( "Historical", "Painter", "Vincent Van Gogh", "A Dutch post-Impressionist painter known for his depictions of wheat fields, sunflowers, and starry nights. He struggled with mental illness, once slicing off his left ear with a razor and delivering it to a brothel as a memento for his friend Paul Gauguin." )
    , ( "Fictional", "Killer", "Hannibal Lector", "A psychiatrist, serial killer, cannibal, and antihero from The Silence of the Lambs novel and film. He was famously portrayed by Anthony Hopkins, who explains to Clarice that he once ate a census taker's liver ''with fava beans and a nice Chianti. [slurping sounds]''" )
    , ( "Historical", "Actor", "Drunk Jeff Goldblum", "An actor who appeared in an Apple ad that was remixed on YouTube to make his speech sound slow and slurred (''What do you think the greatest gift of the holidays is? Internet?''). He has also appeared in The Fly, Jurassic Park, and many other films." )
    , ( "Historical", "Royal", "Princess Di", "The ex-wife of Prince Charles and mother of William and Harry. She died tragically in a Paris car crash, which was found to be the result of grossly negligent driving and to the presence of paparazzi. Her funeral was one of the most viewed events in television history." )
    , ( "Fantasy", "Villain", "The Eye of Sauron", "A manifestation of the title character in J.R.R. Tolkien's fantasy series The Lord of the Rings. Frodo describes it as ''rimmed with fire, but was glazed, yellow as a cat's, watchful and intent, and the black slit of its pupil opened on a pit, a window into nothing." )
    , ( "Historical", "Actor", "William Shatner", "A legendary actor, who portrayed classic characters such as Captain Kirk, TJ Hooker, the guy who saw a plane gremlin on The Twilight Zone, and the Princeline Negotiator. He also has a musical career that began with a spoken word performance of Elton John's ''Rocket Man.''" )
    , ( "Fictional", "Diva", "Miss Piggy", "A porcine Muppet and spouse of Kermit the Frog, who she frequently pounces on. A notorious diva and singer, she is modeled on the singer and actress Peggy Lee. Throughout her career, she has been used to symbolize the modern Americal woman." )
    , ( "Historical", "Actor", "Kevin Bacon", "An actor best known for the game based on six degrees of separation that bears his name, e.g. Andre the Giant and Christopher Guest appeared in The Princess Bride; Christopher Guest and this actor appeared in A Few Good Men; thus Andre the Giant's number is 2)." )
    , ( "Historical", "Painter", "Bob Ross", "A painter and TV host, who starred in PBS's The Joy of Painting. He taught viewers how to paint ''fluffy white clouds'' and ''happy little trees'' using wet on wet painting. He supposedly hated his white-guy afro and kept it only for marketing purposes." )
    , ( "Historical", "Musician", "Sting", "The stage name of Gordon Sumner, former member of The Police and solo musician. Late in his career, he abandoned his rock sound for more mellow instruments like the sitar and lute. He ist also rumored to possess legendary sexual stamina due to his devotion to tantric yoga." )
    , ( "Fictional", "Villain", "Skeletor", "The main villain in the Masters of the Universe fantasy world. He is the arch nemesis of He-Man and is typically shown with a blue humanoid body, bare skull, and purple hood. His goal is to learn the secrets of Castle Grayskull and use them to conquer the land of Eternia." )
    , ( "Historical", "Artist", "Banksy", "The pseudonym of a mysterious UK graffiti artist. His work uses stencils to depict satirical characters, often interacting with the environment. Though nominated for an Academy Award as a documentary, his film Exit Through the Gift Shop is now widely accepted as a hoax." )
    ]


type alias Model =
  { teams : List Team
  , cards : Zip Card
  , state : State
  , maxCount : Int
  , count : Int
  , nextSeed : Seed
  , round : Int
  }

initialModel : Model
initialModel = let (shuffledCards, seed) = Shuffle.rShuffle 3 (initialCards, startTime |> round |> initialSeed)
  in { teams = initialTeams
     , cards = toCards shuffledCards
     , state = Start
     , maxCount = 10
     , count = 10
     , nextSeed = seed
     , round = 1
     }

initialTeams : List Team
initialTeams = [ newTeam "Team Anna", newTeam "Team Bob" ]

activeTeamName : Model -> String
activeTeamName model =
  case model.teams of
    []     -> ""
    x :: _ -> x.name

-- VIEW

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
        , button [ class "later", onClick address Later ] [ text "Later..." ]
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
  li [] [ text (p.name ++ ": " ++ toString p.score) ]

mkTeamInput : Address Action -> (Int, Team) -> Html
mkTeamInput address (i, t) =
  li [] [ input [ type' "text",
                  placeholder t.name,
                  value t.name,
                  name ("team" ++ toString i),
                  autofocus False,
                  onInput address (UpdateName i)] []
          ]

onInput : Address Action -> (String -> Action) -> Attribute
onInput address contentToValue =
  on "input" targetValue (\str -> Signal.message address (contentToValue str))

startView : Address Action -> Model -> Html
startView address model = let indexedPlayers = indexedMap (,) model.teams
  in div [] [ ul     [class "teams"]                              (map (mkTeamInput address) indexedPlayers)
            , button [class "fullPositive", onClick address Init] [ text <| infoTextForRound model]]

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
        [ class "fullPositive", onClick address Unpause ]
        [ text ("Next up: " ++ (activeTeamName model)) ]
    ]

stateView : Address Action -> Model -> Html
stateView address model =
  case model.state of
    Play   -> playView   address model
    Start  -> startView  address model
    Switch -> switchView address model
    End    -> endView    address model

-- UPDATE

incScore : List Team -> List Team
incScore teams =
  case teams of
    p :: ps -> { p | score = p.score + 1 } :: ps
    []      -> teams

type Action
  = NoOp
  | Solved
  | Later
  | Unpause
  | Init
  | NextRound
  | Tick
  | UpdateName Int String

update : Action -> Model -> Model
update action model =
  case action of
    NoOp    -> model
    Init    -> { model | state = Play }
    Later   -> { model | cards = nextElem model.cards }
    Unpause -> { model | state = Play }
    Tick    -> case model.state of
                 Play -> case model.count of
                           0 -> { model | teams = cycle model.teams
                                        , cards = nextElem model.cards
                                        , state = Switch
                                        , count = model.maxCount }
                           n -> { model | count = n - 1 }
                 _    -> model
    Solved  -> case model.cards of
        ( 1, _, _, _ ) -> { model | teams = incScore model.teams
                                  , cards = removeElem model.cards
                                  , round = model.round + 1
                                  , state = End}
        _              -> { model | cards = removeElem model.cards
                                  , teams = incScore model.teams }
    NextRound -> let (shuffledCards, seed) = reshuffle model
                  in {model | state = Play
                            , cards = shuffledCards
                            , nextSeed = seed}
    UpdateName n name -> {model | teams = (updateName n name model.teams)}

updateName : Int -> String -> List Team -> List Team
updateName n name l = let (pre, el, post) = ((take n l), take 1 (drop n l), drop (n + 1) l)
                             in case el of
                              []      -> l
                              e :: _ -> pre ++ [{e | name = name}] ++ post
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
model = foldp update initialModel actions

main : Signal Html
main = Signal.map (view actionsMB.address) model
