{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Program to assign Christmas gifts in a set of person
-- where each person gives exactly one present to another person

module Lib where

import Control.Exception
import Control.Monad.Random.Lazy
import Control.Monad.Trans.Maybe
import qualified Data.Set as S
import Data.Time
import System.Directory
import qualified System.Exit as Exit

-- State encoding that a list of person must give a gift to another person
-- A person cannot give a gift to itself and two persons should not receive a gift
-- from the same person
data PapaState a = PapaState
  { notGiving :: S.Set a,
    presentLess :: [a],
    previous :: S.Set (a, a),
    assignment :: [(a, a)]
  }

instance Show a => Show (PapaState a) where
  show PapaState {assignment} =
    unlines [show giver ++ "->" ++ show receiver | (giver, receiver) <- assignment]

assign0 :: (Ord a, Show a, MonadRandom m) => PapaState a -> MaybeT m (PapaState a)
assign0 state@PapaState {notGiving, presentLess = receiver : receivers, previous, assignment} = do
  guard $ not (null validGivers)
  giver <- uniform validGivers
  return $
    state
      { notGiving = S.delete giver notGiving,
        presentLess = receivers,
        assignment = (giver, receiver) : assignment
      }
  where
    validGivers = S.filter validGiver notGiving
    validGiver giver = S.notMember (giver, receiver) previous && giver /= receiver

assign :: (Ord a, Show a, MonadRandom m) => PapaState a -> MaybeT m (PapaState a)
assign state
  | null (presentLess state) = return state
  | otherwise = assign0 state >>= assign

retryAssignUntilSuccess :: (Ord a, Show a, MonadRandom m) => PapaState a -> m (PapaState a)
retryAssignUntilSuccess state = do
  res <- runMaybeT (assign state)
  case res of
    Nothing -> retryAssignUntilSuccess state
    Just sol -> return sol

data Where = Commercy | George
  deriving (Show)

getPreviousAssignments :: Where -> S.Set (String, String)
getPreviousAssignments location =
  let result = past location
      lengths :: S.Set Int = S.fromList $ map length result -- the lengths of past assignments, should all be the same
      nbLengths = length lengths
   in assert (nbLengths <= 1) (S.fromList $ concat result)
  where
    past :: Where -> [[(String, String)]]
    past Commercy =
      [ [ ("Clement", "Elise"), -- Conjoint contrainte
          ("Henry", " Pascale"),
          ("Elise", "Clement"),
          ("Laura", " Romain"),
          ("Marianne", "Thomas"),
          ("Pascale", " Henry"),
          ("Romain", "Laura"),
          ("Thomas", " Marianne")
        ],
        [ ("Clement", "Thomas"), -- 2019
          ("Henry", " Marianne"),
          ("Elise", "Pascale"),
          ("Laura", " Henry"),
          ("Marianne", "Romain"),
          ("Pascale", " Laura"),
          ("Romain", "Clement"),
          ("Thomas", " Elise")
        ]
      ]
    past George =
      [ -- 2019
        [ ("Alice", "Helene"),
          ("Angelique", "Elisabeth"),
          ("Audrey", "Alice"),
          ("Elisabeth", "Nathalie"),
          ("Elise", "Angelique"),
          ("Helene", "Maie"),
          ("Nathalie", "Audrey"),
          ("Maie", "Elise"),
          ("Perrine", "Sandrine"),
          ("Sandrine", "Perrine"),
          ("JeanDamien", "Annabelle"), -- fake, added in 2020
          ("Annabelle", "Annabelle") -- fake added in 2020
        ],
        -- 2020
        [ ("Alice", "Audrey"),
          ("Angelique", "Helene"),
          ("Annabelle", "Angelique"),
          ("Audrey", "Sandrine"),
          ("Elisabeth", "Perrine"),
          ("Elise", "Elisabeth"),
          ("Helene", "JeanDamien"),
          ("JeanDamien", "Alice"),
          ("Maie", "Nathalie"),
          ("Nathalie", "Maie"),
          ("Perrine", "Elise"),
          ("Sandrine", "Annabelle")
        ]
      ]

getPersons :: Where -> [String]
getPersons Commercy =
  [ "Elise",
    "Clement",
    "Henry",
    "Pascale",
    "Marianne",
    "Thomas",
    "Romain",
    "Laura"
  ]
getPersons George =
  [ "Annabelle",
    "Alice",
    "Angelique",
    "Audrey",
    "Elisabeth",
    "Elise",
    "Helene",
    "JeanDamien",
    "Maie",
    "Nathalie",
    "Perrine",
    "Sandrine"
  ]

main0 :: MonadRandom m => Where -> m (PapaState String)
main0 location =
  retryAssignUntilSuccess $
    PapaState
      { notGiving = S.fromList people,
        presentLess = people,
        previous = getPreviousAssignments location,
        assignment = []
      }
  where
    people = getPersons location

writeGiveToFile ::
  Where ->
  Int -> -- year
  String -> -- giver
  String -> -- given to
  IO ()
writeGiveToFile w year giver recipient = do
  createDirectoryIfMissing False directory
  fileExist <- doesFileExist filepath
  when fileExist $ failWith $ "Not overwriting " ++ filepath
  writeFile filepath recipient
  putStrLn $ "Written " ++ filepath
  where
    yearString = show year
    directory = "resultats-" ++ yearString ++ "-" ++ show w
    filepath = directory ++ "/" ++ giver ++ " donnes à.txt"
    failWith :: String -> IO () = \msg ->
      do
        putStrLn msg
        Exit.exitWith $ Exit.ExitFailure 1

entrypoint :: IO ()
entrypoint = do
  let w :: Where = George
  state :: PapaState String <- main0 w
  c :: UTCTime <- getCurrentTime
  let (y, _, _) = toGregorian $ utctDay c -- (2009,4,21)
      y' :: Int = fromIntegral y
      writeGiveToFile' y p = writeGiveToFile w y (fst p) (snd p)
      effects :: [IO ()] = map (writeGiveToFile' y') (assignment state)
  sequence_ effects

-- print state do not leak result to standard output
