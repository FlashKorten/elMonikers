module ElMonikers.Shuffle (rShuffle) where

import List exposing (..)
import Random exposing (Seed, bool, initialSeed, generate)


rShuffle : Int -> (List a, Seed) -> (List a, Seed)
rShuffle n l = let fs = List.repeat n shuffle
                in l |> foldl (<<) identity fs

shuffle : (List a, Seed) -> (List a, Seed)
shuffle (l, s) =
  if List.length l < 2 then (l, s)
                         else let ( l1, l2 ) = split l
                               in merge ( shuffle (l1, s), shuffle (l2, s) )

merge : ( (List a, Seed), (List a, Seed) ) -> (List a, Seed)
merge input =
  case input of
    ( (xs,s) , ([],_)  ) -> (xs,s)
    ( ([],_) , (ys,s)  ) -> (ys,s)
    ( (x :: xs, s1), (y :: ys, s2) ) -> let (p, sn) = generate bool s1
      in if p then let (m, sx) = (merge ( (xs, s2), ((y :: ys), sn) ))
                    in (x :: m, sx)
              else let (m, sx) = (merge ( ((x :: xs), s2), (ys, sn) ))
                    in (y :: m, sx)

split : List a -> ( List a, List a )
split xs = let go i = case i of
                    ( x::xs, _::_::zs ) -> let ( us, vs ) = go ( xs, zs )
                                            in ( x :: us, vs )
                    ( xs   , _        ) -> ( [], xs )
            in go ( xs, xs )

