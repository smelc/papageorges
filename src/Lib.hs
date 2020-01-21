{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Program to assign Christmas gifts in a set of person
-- where each person gives exactly one present to another person

module Lib where

import Control.Exception
import Control.Monad.State.Lazy
import Data.List
import Debug.Trace
import System.Random

-- State encoding that a list of person must give a gift to another person
-- A person cannot give a gift to itself and two persons should not receive a gift
-- from the same person
data PapaState a = PapaState {
    domain :: [a]            -- ^ The list of persons
    , previous :: [[(a, a)]] -- ^ Past assignments
    , assignment :: [(a, a)] -- ^ The assignment (who gives to who)
}

instance Show a => Show (PapaState a) where
  show PapaState{domain, assignment} = "[" ++ intercalate "," domainStrs ++ "]\n" ++ intercalate "\n" assignStrs
    where domainStrs :: [String] = map show domain
          assignStrs :: [String] = map (\x -> show (fst x) ++ "->" ++ show (snd x)) assignment

sort :: Ord a => PapaState a -> PapaState a
sort PapaState{domain, previous, assignment} =
  PapaState (Data.List.sort domain) previous (sortOn fst assignment)

presentLess :: Eq a => PapaState a -- ^ A state
              -> [a]               -- ^ The list of persons that weren't given a present yet
presentLess PapaState{domain, assignment} = [x | x <- domain, x `notElem` map snd assignment]

notGiving :: Eq a => PapaState a -- ^ A state
           -> [a]                -- ^ The list of persons that do not give a present yet
notGiving PapaState{domain, assignment} = [x | x <- domain, x `notElem` map fst assignment]

assign0 :: (Eq a, Show a) => PapaState a -> State StdGen (PapaState a)
assign0 state = do
  dice <- get
  let (giverIndex, dice') = randomR (0, length candidateGivers - 1) dice
      assign = assignment state
      assign' = (candidateGivers !! giverIndex, receiver) : assign
      state'  = state { assignment = assign' }
  put dice'
  return state'
  where job = presentLess state
        receiver = head job
        allPrevious = concat $ previous state
        candidateGivers = [x | x <- notGiving state, (x, receiver) `notElem` allPrevious && x /= receiver]

assign :: (Eq a, Show a) => StdGen -> PapaState a -> PapaState a
assign rng state =
  if null $ presentLess state
  then state
  else let (state', rng') = runState (assign0 state) rng in
       assign rng' state'

data Where = Commercy | George

getPreviousAssignments :: Where -> [[(String, String)]]
getPreviousAssignments location =
  let
    result = past location
    lengths :: [Int] = map length result -- the lengths of past assignments, should all be the same
    nbLengths = length lengths
  in
    assert (nbLengths <= 1) result
  where
    past :: Where -> [[(String, String)]]
    past Commercy = [
      [("Clement", "Thomas"), -- 2019
       ("Henry", " Marianne"),
       ("Elise", "Pascale"),
       ("Laura", " Henry"),
       ("Marianne", "Romain"),
       ("Pascale", " Laura"),
       ("Romain", "Clement"),
       ("Thomas", " Elise")
      ]]
    past George = []

getPersons :: Where -> [String]
getPersons Commercy = ["Elise", "Clement", "Henry", "Pascale", "Marianne",
                       "Thomas", "Romain", "Laura"]
getPersons George = []

main0 :: StdGen -> Where -> PapaState String
main0 rng location =
  Lib.sort $ assign rng initialState
  where
    pastAssignments = getPreviousAssignments location
    domain = getPersons location
    initialState = PapaState domain pastAssignments []

entrypoint :: IO ()
entrypoint = do
    g :: StdGen <- newStdGen
    print $ main0 g Commercy
    return ()