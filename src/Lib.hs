{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Program to assign Christmas gifts in a set of person
-- where each person gives exactly one present to another person

module Lib where

import Control.Monad.State
import Data.List
import Debug.Trace
import System.Random

-- State encoding that a list of person must give a gift to another person
-- A person cannot give a gift to itself and two persons should not receive a gift
-- from the same person
data PapaState a = PapaState {
    domain :: [a]            -- ^ The list of persons
    , assignment :: [(a, a)] -- ^ The assignment (who gives to who)
    , rng :: StdGen          -- ^ The random number generator
}

instance Show a => Show (PapaState a) where
  show PapaState{domain, assignment} = "[" ++ intercalate "," domainStrs ++ "]\n" ++ intercalate "\n" assignStrs
    where domainStrs :: [String] = map show domain
          assignStrs :: [String] = map (\x -> show (fst x) ++ "->" ++ show (snd x)) assignment

presentLess :: Eq a => PapaState a -- ^ A state
              -> [a]               -- ^ The list of persons that weren't given a present yet
presentLess PapaState{domain, assignment} = [x | x <- domain, x `notElem` map snd assignment]

notGiving :: Eq a => PapaState a -- ^ A state
           -> [a]                -- ^ The list of persons that do not give a present yet
notGiving PapaState{domain, assignment} = [x | x <- domain, x `notElem` map fst assignment]

assign0 :: Eq a => Show a => PapaState a -> PapaState a
assign0 state =
  let receiver = head job
      dice = rng state
      (giverIndex, dice') = randomR (0, length candidateGivers - 1) dice
      assign = assignment state
      assign' = (candidateGivers !! giverIndex, receiver) : assign
      state'  = state { assignment = assign', rng = dice'}
      in state'
  where job = presentLess state
        candidateGivers = notGiving state

assign :: Eq a => Show a => PapaState a -> PapaState a
assign = until (null . presentLess) assign0

nbPersons = 5

entrypoint :: IO ()
entrypoint = do
    g <- newStdGen
    let (v :: Int, g') = randomR (1, 10) g
        domain = [0 .. nbPersons - 1]
        initialState = PapaState domain [] g
        result = assign initialState
    print result
    return ()