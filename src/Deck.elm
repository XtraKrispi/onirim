module Deck exposing
    ( Deck
    , add
    , draw
    , isEmpty
    , new
    , shuffle
    , shuffleGen
    )

import Random
import Random.List as R


type Deck a
    = Deck (List a)


new : List a -> Deck a
new =
    Deck


shuffleGen : Deck a -> Random.Generator (Deck a)
shuffleGen (Deck xs) =
    xs
        |> R.shuffle
        |> Random.map Deck


add : List a -> Deck a -> Deck a
add xs (Deck d) =
    Deck (xs ++ d)


shuffle : Random.Seed -> Deck a -> ( Deck a, Random.Seed )
shuffle seed d =
    Random.step (shuffleGen d) seed


draw : Int -> Deck a -> { cards : List a, deck : Deck a }
draw n (Deck xs) =
    { cards = List.take n xs
    , deck = Deck (List.drop n xs)
    }


isEmpty : Deck a -> Bool
isEmpty (Deck xs) =
    List.isEmpty xs
