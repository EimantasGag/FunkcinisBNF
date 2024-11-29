{-# LANGUAGE InstanceSigs #-}
module Lib3
    ( stateTransition,
    StorageOp (..),
    Command (..),
    Statements (..),
    storageOpLoop,
    parseCommand,
    parseStatements,
    marshallState,
    renderStatements,
    ) where

import Control.Concurrent ( Chan, readChan, writeChan, newChan )
import Control.Concurrent.STM(STM, TVar, readTVar, readTVarIO, writeTVar, atomically)
import qualified Lib2
import System.IO.Error (catchIOError)

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | This function is started from main
-- in a dedicated thread. It must be used to control
-- file access in a synchronized manner: read requests
-- from chan, do the IO operations needed and respond
-- to a channel provided in a request.
-- Modify as needed.
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop channel = do
  msg <- readChan channel

  case msg of
    Save dataToSave channel2 -> do
      writeFile "savefile.txt" dataToSave
      writeChan channel2()
    Load channel2 -> do
      contents <- catchIOError (readFile "savefile.txt") (\_ -> return "") 
      writeChan channel2 contents

  storageOpLoop channel

data Statements = Batch [Lib2.Query] |
               Single Lib2.Query
               deriving (Show, Eq)

data Command = StatementCommand Statements |
               LoadCommand |
               SaveCommand
               deriving (Show, Eq)

-- | Parses user's input.
parseCommand :: String -> Either String (Command, String)
parseCommand input = 
  case Lib2.parseWord input of
    Right (word, rest) -> case word of
      "save" -> if rest == "" then Right (SaveCommand, rest) else Left ("Unexpected characters save command: " ++ rest)
      "load" -> if rest == "" then Right (LoadCommand, rest) else Left ("Unexpected characters load command: " ++ rest)
      _ -> case parseStatements input of
        Right(statement, rest) -> Right(StatementCommand statement, rest)
        Left err -> Left err
    Left e -> Left e

-- | Parses Statement.
-- Must be used in parseCommand.
-- Reuse Lib2 as much as you can.
-- You can change Lib2.parseQuery signature if needed.
parseStatements :: String -> Either String (Statements, String)
parseStatements input = 
  case Lib2.parseWord input of
    Right(word, rest) -> case word of
      "BEGIN" -> case take 1 rest of 
        " " -> case parseBatch (drop 1 rest) [] of
          Right batch -> Right(Batch batch, "")
          Left err -> Left err
        _ -> Left "Expected whitespace after BEGIN"
      _ -> case Lib2.parseQuery input of
        Right query -> Right(Single query, "")
        Left err -> Left err
    Left e -> Left e

-- Grazina Left err, arba Right (batcho stringas, ir rest stringas)
getOneBatchString :: String -> String -> Either String (String, String)
getOneBatchString "" _ = Left "Batch has to end with 'END'" 
getOneBatchString " END" res = Right (res, "") 
getOneBatchString input res = 
  case take 1 input of
    ";" -> Right (res, drop 1 input)
    _ -> getOneBatchString (drop 1 input) (res ++ take 1 input)

parseBatch :: String -> [Lib2.Query] -> Either String [Lib2.Query]
parseBatch "" batch = Right batch
parseBatch input batch = case getOneBatchString input "" of
  Right (batchString, rest) -> 
    case Lib2.parseQuery batchString of
      Right query -> 
        if take 1 rest == " " || take 1 rest == "" then parseBatch (drop 1 rest) (batch ++ [query]) else Left "Expected white space after ;"
      Left err -> Left err
  Left err -> Left err


orderListToBatch :: [Lib2.Order] -> [Lib2.Query] -> Statements
orderListToBatch [] batch = Batch batch 
orderListToBatch ordersList batch = orderListToBatch (tail ordersList) (batch ++ [Lib2.CreateOrder (head ordersList)])

-- | Converts program's state into Statements
-- (probably a batch, but might be a single query)
marshallState :: Lib2.State -> Statements
marshallState Lib2.State{Lib2.ordersList=ordersList} = orderListToBatch ordersList []

queriesToBatchString :: [Lib2.Query] -> String -> String
queriesToBatchString [] res = res ++ " END"
queriesToBatchString (h:t) res =
  case h of
    Lib2.CreateOrder order -> 
      if null t then queriesToBatchString t (res ++ show order) else queriesToBatchString t (res ++ show order ++ "; ")
    _ -> queriesToBatchString t res

-- | Renders Statements into a String which
-- can be parsed back into Statements by parseStatements
-- function. The String returned by this function must be used
-- as persist program's state in a file. 
-- Must have a property test
-- for all s: parseStatements (renderStatements s) == Right(s, "")
renderStatements :: Statements -> String
renderStatements statements = case statements of
  Batch batch -> if null batch then "" else queriesToBatchString batch "BEGIN "
  Single single -> queriesToBatchString [single] "BEGIN "

-- | Updates a state according to a command.
-- Performs file IO via ioChan if needed.
-- This allows your program to share the state
-- between repl iterations, save the state to a file,
-- load the state from the file so the state is preserved
-- between program restarts.
-- Keep IO as small as possible.
-- State update must be executed atomically (STM).
-- Right contains an optional message to print, updated state
-- is stored in transactinal variable

stateTransitionBatch :: TVar Lib2.State -> Lib2.State -> [Lib2.Query] -> STM (Either String (Maybe String))
stateTransitionBatch state curState [] = return $ Left "SHOULD NOT REACH THIS"
stateTransitionBatch state curState (h:t) =
  case Lib2.stateTransition curState h of
    Right (msg, changedState) -> 
      do 
        if null t then do
          writeTVar state changedState
          return $ Right msg
        else stateTransitionBatch state changedState t
    Left err -> return $ Left err

stateTransition :: TVar Lib2.State -> Command -> Chan StorageOp ->
                   IO (Either String (Maybe String))
stateTransition state command ioChan = 
  case command of
    StatementCommand statement -> case statement of
      Batch batch -> 
        do
          curState <- readTVarIO state
          atomically $ stateTransitionBatch state curState batch
      Single single -> 
        do
          curState <- readTVarIO state
          atomically $ stateTransitionBatch state curState [single]
    SaveCommand -> do 
      saveChannel <- newChan
      curState <- readTVarIO state
      writeChan ioChan (Save (renderStatements (marshallState curState)) saveChannel)
      return $ Right (Just "State successfully saved in file")
    LoadCommand -> do
      loadChannel <- newChan
      writeChan ioChan (Load loadChannel)
      stateFromFile <- readChan loadChannel

      curState <- readTVarIO state

      case parseStatements stateFromFile of
        Right (stat, _) -> case stat of
          Batch batch -> do
            _ <- atomically $ stateTransitionBatch state curState batch
            return $ Right (Just "State successfully loaded from file")
          Single single -> do
            _ <- atomically $ stateTransitionBatch state curState [single]
            return $ Right (Just "State successfully loaded from file")
        Left _ -> return $ if null stateFromFile then Left "No state saved in file" else Left "File corrupted"

