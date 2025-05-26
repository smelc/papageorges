{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- Program to assign Christmas gifts in a set of person
-- where each person gives exactly one present to another person

module Lib where

import Control.Exception
import Control.Monad.Random.Lazy
import Control.Monad.Trans.Maybe
import Data.List (delete, intersperse)
import qualified Data.Set as S
import Data.Time
import System.Directory
import qualified System.Environment as Env
import qualified System.Exit as Exit
import System.IO (hPutStrLn, stderr)
import System.Random.Shuffle

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

pickAll :: (MonadRandom m) => PapaState String -> m (PapaState String)
pickAll state@PapaState{notGiving, presentLess, previous, assignment} = do
  pure $ assert ((S.size notGiving) == (length presentLess))
  case null notGiving of
    True -> return state -- We're done
    False -> do
      possibleGivers :: [String] <- shuffleM $ S.toList notGiving
      case possibleGivers of
        [] -> error "Impossible possibleGivers"
        giver : _ -> do
          let notGiving' :: S.Set String = S.delete giver notGiving
              possibleReceivers = [p | p <- presentLess, (giver, p) `S.notMember` previous, (giver, p) `notElem` assignment]
          possibleReceivers' <- shuffleM possibleReceivers
          case possibleReceivers' of
            [] -> error "Impossible possibleReceivers"
            receiver : _ -> do
              let presentLess' :: [String] = delete receiver presentLess
                  assignment' = (giver, receiver) : assignment
                  state' :: PapaState String = PapaState notGiving' presentLess' previous assignment'
              pickAll state'

data Where = Commercy | Georges
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
      [ [ ("Clement", "Elise"), -- Contrainte conjoint
          ("Henry", "Pascale"),
          ("Elise", "Clement"),
          ("Laura", "Romain"),
          ("Marianne", "Thomas"),
          ("Pascale", "Henry"),
          ("Romain", "Laura"),
          ("Thomas", "Marianne")
        ],
        [ ("Clement", "Clement"), -- Contrainte soi-même
          ("Henry", "Henry"),
          ("Elise", "Elise"),
          ("Laura", "Laura"),
          ("Marianne", "Marianne"),
          ("Pascale", "Pascale"),
          ("Romain", "Romain"),
          ("Thomas", "Thomas")
        ],
        -- [ ("Clement", "Thomas"), -- 2019
        --   ("Henry", "Marianne"),
        --   ("Elise", "Pascale"),
        --   ("Laura", "Henry"),
        --   ("Marianne", "Romain"),
        --   ("Pascale", "Laura"),
        --   ("Romain", "Clement"),
        --   ("Thomas", "Elise")
        -- ],
        -- [ ("Clement", "Laura"), -- 2020
        --   ("Henry", "Romain"),
        --   ("Elise", "Marianne"),
        --   ("Laura", "Elise"),
        --   ("Marianne", "Pascale"),
        --   ("Pascale", "Thomas"),
        --   ("Romain", "Henry"),
        --   ("Thomas", "Clement")
        -- ],
        -- [ ("Clement", "Henry"), -- 2021
        --   ("Henry", "Clement"),
        --   ("Elise", "Romain"),
        --   ("Laura", "Marianne"),
        --   ("Marianne", "Laura"),
        --   ("Pascale", "Thomas"),
        --   ("Romain", "Pascale"),
        --   ("Thomas", "Elise")
        -- ],
        -- [ ("Clement", "Marianne"), -- 2022
        --   ("Elise", "Laura"),
        --   ("Henry", "Elise"),
        --   ("Laura", "Clement"),
        --   ("Marianne", "Henry"),
        --   ("Pascale", "Romain"),
        --   ("Romain", "Thomas"),
        --   ("Thomas", "Pascale")
        -- ],
        -- [ ("Clement", "Pascale"), -- 2023
        --   ("Elise", "Henry"),
        --   ("Henry", "Laura"),
        --   ("Laura", "Thomas"),
        --   ("Marianne", "Clement"),
        --   ("Pascale", "Marianne"),
        --   ("Romain", "Elise"),
        --   ("Thomas", "Romain")
        -- ],
        -- [ ("Clement", "Romain"), -- 2024
        --   ("Elise", "Thomas"),
        --   ("Henry", "Marianne"),
        --   ("Laura", "Pascale"),
        --   ("Marianne", "Elise"),
        --   ("Pascale", "Laura"),
        --   ("Romain", "Clement"),
        --   ("Thomas", "Henry")
        -- ]
        [ ("Clement", "Marianne"), -- 2025
          ("Henry", "Laura"),
          ("Elise", "Thomas"),
          ("Laura", "Elise"),
          ("Marianne", "Clement"),
          ("Pascale", "Romain"),
          ("Romain", "Henry"),
          ("Thomas", "Pascale")
        ]
      ]
    past Georges =
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
          ("Sandrine", "Perrine")
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
getPersons Georges =
  [ -- "Annabelle",
    "Alice",
    "Angelique",
    "Audrey",
    "Elisabeth",
    "Elise",
    "Helene",
    -- "JeanDamien",
    "Maie",
    "Nathalie",
    "Perrine",
    "Sandrine"
  ]

main0 :: MonadRandom m => Where -> m (PapaState String)
main0 location =
  pickAll $
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
  (String, String) -> -- giver, given to
  IO ()
writeGiveToFile w year (giver, recipient) = do
  createDirectoryIfMissing False directory
  fileExist <- doesFileExist filepath
  when fileExist $ failWith $ "Not overwriting " ++ filepath
  writeFile filepath recipient
  putStrLn $ "Written " ++ filepath
  where
    yearString = show year
    directory = "resultats-" ++ yearString ++ "-" ++ show w
    filepath = directory ++ "/" ++ giver ++ " donne à.txt"
    failWith :: String -> IO () = \msg ->
      do
        putStrLn msg
        Exit.exitWith $ Exit.ExitFailure 1

parseArgs :: [String] -> Either String Where
parseArgs ["Commercy"] = Right Commercy
parseArgs ["Georges"] = Right Georges
parseArgs _ = Left "Exactly one argument must be passed: \"Commercy\" or \"Georges\""

entrypoint :: IO ()
entrypoint = do
  w <- parseWhere
  state :: PapaState String <- main0 w
  c :: UTCTime <- getCurrentTime
  let (y, _, _) = toGregorian $ utctDay c -- (2009,4,21)
      y' :: Int = fromIntegral y
      effects :: [IO ()] = map (writeGiveToFile w y') (assignment state)
  sequence_ effects
  where
    parseWhere :: IO Where = do
      args <- Env.getArgs
      case parseArgs args of
        Left msg -> do
          hPutStrLn stderr $ "Cannot parse command line arguments: " ++ msg
          Exit.exitWith $ Exit.ExitFailure 1
        Right r ->
          return r

-- print state do not leak result to standard output
