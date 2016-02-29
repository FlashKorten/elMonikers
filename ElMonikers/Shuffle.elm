module ElMonikers.Shuffle (rShuffle) where

import List exposing (foldl, repeat, length)
import Random exposing (Seed, bool, generate)

rShuffle : Int -> (List a, Seed) -> (List a, Seed)
rShuffle n l = let fs = repeat n shuffle
                in l |> foldl (<<) identity fs

shuffle : (List a, Seed) -> (List a, Seed)
shuffle (l, seed) =
  if length l < 2 then (l, seed)
                  else let ( h1, h2 ) = split l
                           ( shuffled1, tmpSeed) = shuffle (h1, seed)
                           ( shuffled2, newSeed) = shuffle (h2, tmpSeed)
                        in merge (shuffled1, shuffled2, newSeed)

merge : (List a, List a, Seed) -> (List a, Seed)
merge t = case t of
    ( xs,    [],    seed ) -> (xs, seed)
    ( [],    ys,    seed ) -> (ys, seed)
    ( x::xs, y::ys, seed ) -> let (p, tmpSeed) = generate bool seed
      in if p then let (m, newSeed) = merge (xs, y :: ys, tmpSeed)
                    in (x::m, newSeed)
              else let (m, newSeed) = merge (x :: xs, ys, tmpSeed)
                    in (y::m, newSeed)

split : List a -> ( List a, List a )
split xs = let go i = case i of
                    ( x::xs, _::_::zs ) -> let ( us, vs ) = go ( xs, zs )
                                            in ( x :: us, vs )
                    ( xs   , _        ) -> ( [], xs )
            in go ( xs, xs )

