{-# OPTIONS_GHC -Wno-orphans #-}
module Lib3(
    emptyState, State(..), execute, load, save, storageOpLoop, StorageOp, Parser(..), parseCommand) where

import qualified Lib1
import Control.Concurrent.STM.TVar (TVar, readTVar, writeTVar)
import Control.Concurrent.STM (atomically)
import Control.Concurrent (Chan, readChan, writeChan, newChan)
import System.IO.Strict (readFile)
import System.IO (writeFile)
import Control.Applicative (Alternative(..))
import qualified Data.Char as C
import Prelude hiding (readFile, writeFile)
import qualified Prelude

-- | Parser type with Functor, Applicative, Alternative instances
newtype Parser a = Parser {
    runParser :: String -> Either String (a, String)
}

-- PARSER INSTANCES

instance Functor Parser where
  fmap f (Parser p) = Parser $ \input -> case p input of
    Left err -> Left err
    Right (v, rest) -> Right (f v, rest)

instance Applicative Parser where
  pure v = Parser $ \input -> Right (v, input)
  
  (Parser pf) <*> (Parser pa) = Parser $ \input -> case pf input of
    Left err -> Left err
    Right (f, rest1) -> case pa rest1 of
      Left err -> Left err
      Right (a, rest2) -> Right (f a, rest2)

instance Alternative Parser where
  empty = Parser $ \_ -> Left "empty parser"
  
  (Parser p1) <|> (Parser p2) = Parser $ \input -> case p1 input of
    Right result -> Right result
    Left _ -> p2 input

-- PRIMITIVE PARSERS

parseChar :: Char -> Parser Char
parseChar expected = Parser $ \input -> case input of
  [] -> Left $ "Expected '" ++ [expected] ++ "', got end of input"
  (c:cs) -> if c == expected
            then Right (c, cs)
            else Left $ "Expected '" ++ [expected] ++ "', got '" ++ [c] ++ "'"

parseWhitespace :: Parser String
parseWhitespace = Parser $ \input -> Right (span isWhitespace input)
  where
    isWhitespace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

parseWhitespace1 :: Parser String
parseWhitespace1 = Parser $ \input -> case runParser parseWhitespace input of
  Right ("", _) -> Left "Expected whitespace"
  result -> result

-- Parse quoted string: "text"
parseString :: Parser String
parseString = Parser $ \input -> case input of
  [] -> Left "Expected '\"', got end of input"
  ('"':rest) -> parseStringContent rest ""
  (c:_) -> Left $ "Expected '\"', got '" ++ [c] ++ "'"
  where
    parseStringContent [] _ = Left "Unexpected end of string"
    parseStringContent ('"':rest) acc = Right (reverse acc, rest)
    parseStringContent ('\\':c:rest) acc = parseStringContent rest (c:acc)
    parseStringContent (c:rest) acc = parseStringContent rest (c:acc)

parseStringLiteral :: String -> Parser String
parseStringLiteral expected = Parser $ \input -> 
  if take (length expected) input == expected
  then Right (expected, drop (length expected) input)
  else Left $ "Expected '" ++ expected ++ "'"

-- <taskId> ::= [a-zA-Z0-9._]+
parseTaskId :: Parser String
parseTaskId = Parser $ \input -> case parseTaskIdContent input "" of
  ([], _) -> Left "Expected task ID"
  (taskId, rest) -> Right (taskId, rest)
  where
    parseTaskIdContent [] acc = (reverse acc, [])
    parseTaskIdContent (c:cs) acc = 
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || 
         (c >= '0' && c <= '9') || c == '.' || c == '_'
      then parseTaskIdContent cs (c:acc)
      else (reverse acc, c:cs)

-- COMMAND PARSERS

-- add task <taskId> <string>
parseAddTask :: Parser Lib1.Command
parseAddTask = 
  Lib1.AddTask 
    <$> (parseStringLiteral "add task" *> parseWhitespace1 *> parseTaskId)
    <*> (parseWhitespace1 *> parseString)

-- add subtask <taskId> <string> to <taskId>
parseAddSubtask :: Parser Lib1.Command
parseAddSubtask = 
  (\sid desc pid -> Lib1.AddSubtask sid desc pid)
    <$> (parseStringLiteral "add subtask" *> parseWhitespace1 *> parseTaskId)
    <*> (parseWhitespace1 *> parseString)
    <*> (parseWhitespace1 *> parseStringLiteral "to" *> parseWhitespace1 *> parseTaskId)

-- remove task <taskId>
parseRemoveTask :: Parser Lib1.Command
parseRemoveTask = 
  Lib1.RemoveTask 
    <$> (parseStringLiteral "remove task" *> parseWhitespace1 *> parseTaskId)

-- list tasks [<taskId>]?
parseListTasks :: Parser Lib1.Command
parseListTasks = 
  parseStringLiteral "list tasks" *> parseWhitespace *>
    (   (Lib1.ListTasks . Just <$> parseTaskId)
    <|> pure (Lib1.ListTasks Nothing))

-- complete task <taskId> [recursively]?
parseCompleteTask :: Parser Lib1.Command
parseCompleteTask = 
  Lib1.CompleteTask
    <$> (parseStringLiteral "complete task" *> parseWhitespace1 *> parseTaskId)
    <*> (   (parseWhitespace1 *> parseStringLiteral "recursively" *> pure True)
        <|> pure False)

-- report tasks
parseReportTasks :: Parser Lib1.Command
parseReportTasks = 
  parseStringLiteral "report tasks" *> pure Lib1.ReportTasks

-- dump examples
parseDump :: Parser Lib1.Command
parseDump = 
  parseStringLiteral "dump examples" *> pure (Lib1.Dump Lib1.Examples)

-- Main command parser
parseCommand :: Parser Lib1.Command
parseCommand = 
  parseAddTask
  <|> parseAddSubtask
  <|> parseRemoveTask
  <|> parseListTasks
  <|> parseCompleteTask
  <|> parseReportTasks
  <|> parseDump

-- STATE DEFINITION

data State = State
  { tasks :: [Task]
  } deriving (Show, Eq)

data Task = Task
  { taskId       :: String
  , taskDesc     :: String
  , taskStatus   :: Bool
  , taskSubtasks :: [Task]
  } deriving (Show, Eq)

emptyState :: State
emptyState = State []

-- EXECUTE FUNCTION

-- | Business logic with single atomically call
execute :: TVar State -> Lib1.Command -> IO ()
execute stateVar cmd = do
  output <- atomically $ do
    currentState <- readTVar stateVar
    let (newState, outputMsg) = executeCommand currentState cmd
    writeTVar stateVar newState
    return outputMsg
  
  -- Print output if any
  case output of
    Nothing -> return ()
    Just msg -> putStrLn msg

-- Pure state transition logic
executeCommand :: State -> Lib1.Command -> (State, Maybe String)
executeCommand state cmd = case cmd of
  Lib1.AddTask tid desc -> 
    if taskExists tid (tasks state)
    then (state, Just $ "Error: Task with ID '" ++ tid ++ "' already exists")
    else (State (tasks state ++ [Task tid desc False []]), Just $ "Task '" ++ tid ++ "' added")
  
  Lib1.AddSubtask sid desc pid -> 
    case addSubtaskToParent sid desc pid (tasks state) of
      Nothing -> (state, Just $ "Error: Parent task '" ++ pid ++ "' not found or subtask already exists")
      Just newTasks -> (State newTasks, Just $ "Subtask '" ++ sid ++ "' added to '" ++ pid ++ "'")
  
  Lib1.RemoveTask tid -> 
    if not $ taskExists tid (tasks state)
    then (state, Just $ "Error: Task '" ++ tid ++ "' not found")
    else (State (removeTask tid (tasks state)), Just $ "Task '" ++ tid ++ "' removed")
  
  Lib1.ListTasks Nothing -> 
    (state, Just $ renderTasks (tasks state) 0)
  
  Lib1.ListTasks (Just tid) -> 
    case findTask tid (tasks state) of
      Nothing -> (state, Just $ "Error: Task '" ++ tid ++ "' not found")
      Just task -> (state, Just $ renderTask task 0)
  
  Lib1.CompleteTask tid recursive -> 
    case completeTask tid recursive (tasks state) of
      Nothing -> (state, Just $ "Error: Task '" ++ tid ++ "' not found")
      Just newTasks -> (State newTasks, Just $ "Task '" ++ tid ++ "' completed" ++ if recursive then " (recursively)" else "")
  
  Lib1.ReportTasks -> 
    let report = generateReport (tasks state)
    in (state, Just report)
  
  Lib1.Dump Lib1.Examples -> 
    (state, Just $ unlines $ map renderCommand Lib1.examples)

-- HELPER FUNCTIONS FOR TASK OPERATIONS

taskExists :: String -> [Task] -> Bool
taskExists tid tasks = any (\t -> taskId t == tid || taskExistsIn tid (taskSubtasks t)) tasks
  where
    taskExistsIn tid subtasks = any (\t -> taskId t == tid || taskExistsIn tid (taskSubtasks t)) subtasks

findTask :: String -> [Task] -> Maybe Task
findTask tid [] = Nothing
findTask tid (t:ts) 
  | taskId t == tid = Just t
  | otherwise = case findTask tid (taskSubtasks t) of
      Just found -> Just found
      Nothing -> findTask tid ts

addSubtaskToParent :: String -> String -> String -> [Task] -> Maybe [Task]
addSubtaskToParent sid desc pid tasks = mapM (addToTask sid desc pid) tasks
  where
    addToTask sid desc pid task
      | taskId task == pid = 
          if taskExists sid [task]
          then Nothing
          else Just $ task { taskSubtasks = taskSubtasks task ++ [Task sid desc False []] }
      | otherwise = do
          newSubtasks <- addSubtaskToParent sid desc pid (taskSubtasks task)
          return $ task { taskSubtasks = newSubtasks }

removeTask :: String -> [Task] -> [Task]
removeTask tid tasks = mapMaybe removeFromTask tasks
  where
    removeFromTask task
      | taskId task == tid = Nothing
      | otherwise = Just $ task { taskSubtasks = removeTask tid (taskSubtasks task) }
    
    mapMaybe :: (a -> Maybe a) -> [a] -> [a]
    mapMaybe _ [] = []
    mapMaybe f (x:xs) = case f x of
      Nothing -> mapMaybe f xs
      Just y -> y : mapMaybe f xs

completeTask :: String -> Bool -> [Task] -> Maybe [Task]
completeTask tid recursive tasks = mapM (completeInTask tid recursive) tasks
  where
    completeInTask tid recursive task
      | taskId task == tid = 
          if recursive
          then Just $ task { taskStatus = True, taskSubtasks = completeAllSubtasks (taskSubtasks task) }
          else Just $ task { taskStatus = True }
      | otherwise = do
          newSubtasks <- completeTask tid recursive (taskSubtasks task)
          return $ task { taskSubtasks = newSubtasks }
    
    completeAllSubtasks = map (\t -> t { taskStatus = True, taskSubtasks = completeAllSubtasks (taskSubtasks t) })

renderTasks :: [Task] -> Int -> String
renderTasks [] _ = "No tasks"
renderTasks tasks indent = unlines $ map (\t -> renderTask t indent) tasks

renderTask :: Task -> Int -> String
renderTask task indent = 
  let prefix = replicate indent ' '
      status = if taskStatus task then "[X]" else "[ ]"
      line = prefix ++ status ++ " " ++ taskId task ++ ": " ++ taskDesc task
      subtaskLines = if null (taskSubtasks task)
                     then ""
                     else renderTasks (taskSubtasks task) (indent + 2)
  in if null (taskSubtasks task)
     then line
     else line ++ "\n" ++ subtaskLines

generateReport :: [Task] -> String
generateReport tasks = 
  let (total, completed) = countTasks tasks
      percentage = if total == 0 then 0 else (completed * 100) `div` total
  in "Task Report:\n" ++
     "Total tasks: " ++ show total ++ "\n" ++
     "Completed: " ++ show completed ++ "\n" ++
     "Pending: " ++ show (total - completed) ++ "\n" ++
     "Completion: " ++ show percentage ++ "%"
  where
    countTasks [] = (0, 0)
    countTasks (t:ts) = 
      let (subTotal, subCompleted) = countTasks (taskSubtasks t)
          (restTotal, restCompleted) = countTasks ts
          thisCompleted = if taskStatus t then 1 else 0
      in (1 + subTotal + restTotal, thisCompleted + subCompleted + restCompleted)

-- STORAGE OPERATIONS

data StorageOp = Save String (Chan ()) | Load (Chan String)

-- | Storage operation loop - runs forever
storageOpLoop :: Chan StorageOp -> IO ()
storageOpLoop chan = do
  -- Create empty state file if it doesn't exist
  appendFile "state.txt" ""
  loop
  where
    loop = do
      op <- readChan chan
      case op of
        Save content responseChan -> do
          Prelude.writeFile "state.txt" content
          writeChan responseChan ()
          loop
        
        Load responseChan -> do
          content <- readFile "state.txt"
          writeChan responseChan content
          loop

-- | Save state to file through channel
save :: Chan StorageOp -> TVar State -> IO (Either String ())
save chan stateVar = do
  currentState <- atomically $ readTVar stateVar
  let commands = marshallState currentState
  let content = renderCommands commands
  
  responseChan <- newChan
  writeChan chan (Save content responseChan)
  _ <- readChan responseChan
  return $ Right ()

-- | Load state from file through channel
load :: Chan StorageOp -> TVar State -> IO (Either String ())
load chan stateVar = do
  responseChan <- newChan
  writeChan chan (Load responseChan)
  content <- readChan responseChan
  
  case parseStatements content of
    Left err -> return $ Left err
    Right commands -> do
      atomically $ writeTVar stateVar (applyCommands emptyState commands)
      return $ Right ()

-- Parse multiple statements (commands separated by newlines)
parseStatements :: String -> Either String [Lib1.Command]
parseStatements input = 
  let commandLines = lines input
      nonEmpty = filter (not . null . dropWhile C.isSpace) commandLines
  in mapM parseSingleCommand nonEmpty
  where
    parseSingleCommand :: String -> Either String Lib1.Command
    parseSingleCommand cmd = case runParser parseCommand cmd of
      Left err -> Left err
      Right (command, rest) -> 
        if all C.isSpace rest
        then Right command
        else Left $ "Unexpected input after command: " ++ rest

-- Apply a list of commands to state
applyCommands :: State -> [Lib1.Command] -> State
applyCommands = foldl (\s cmd -> fst $ executeCommand s cmd)

-- STATE MARSHALLING

-- Convert state to list of commands
marshallState :: State -> [Lib1.Command]
marshallState (State tasks) = marshallTasks tasks []
  where
    marshallTasks :: [Task] -> [String] -> [Lib1.Command]
    marshallTasks [] _ = []
    marshallTasks (t:ts) parentPath = 
      let addCmd = if null parentPath
                   then Lib1.AddTask (taskId t) (taskDesc t)
                   else Lib1.AddSubtask (taskId t) (taskDesc t) (last parentPath)
          subtaskCmds = marshallTasks (taskSubtasks t) (parentPath ++ [taskId t])
          completeCmd = if taskStatus t 
                        then [Lib1.CompleteTask (taskId t) False]
                        else []
      in addCmd : (subtaskCmds ++ completeCmd ++ marshallTasks ts parentPath)

-- Render commands to string
renderCommands :: [Lib1.Command] -> String
renderCommands cmds = unlines $ map renderCommand cmds

renderCommand :: Lib1.Command -> String
renderCommand (Lib1.AddTask tid desc) = 
  "add task " ++ tid ++ " \"" ++ escapeString desc ++ "\""
renderCommand (Lib1.AddSubtask sid desc pid) = 
  "add subtask " ++ sid ++ " \"" ++ escapeString desc ++ "\" to " ++ pid
renderCommand (Lib1.RemoveTask tid) = 
  "remove task " ++ tid
renderCommand (Lib1.ListTasks Nothing) = 
  "list tasks"
renderCommand (Lib1.ListTasks (Just tid)) = 
  "list tasks " ++ tid
renderCommand (Lib1.CompleteTask tid True) = 
  "complete task " ++ tid ++ " recursively"
renderCommand (Lib1.CompleteTask tid False) = 
  "complete task " ++ tid
renderCommand Lib1.ReportTasks = 
  "report tasks"
renderCommand (Lib1.Dump Lib1.Examples) = 
  "dump examples"

-- Escape special characters in strings
escapeString :: String -> String
escapeString = concatMap escapeChar
  where
    escapeChar '"' = "\\\""
    escapeChar '\\' = "\\\\"
    escapeChar c = [c]