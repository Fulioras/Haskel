<<<<<<< HEAD
  module Lib1
=======
module Lib1
>>>>>>> upstream/main
    ( examples, Command(..), Dumpable(..)
    ) where

data Dumpable = Examples
  deriving Show

-- This is a "root" ADT representing your grammar,
-- Please expand this ADT as needed
data Command 
  = AddTask String String
  | AddSubtask String String String
  | RemoveTask String
  | ListTasks (Maybe String)
  | CompleteTask String Bool
  | ReportTasks
  | Dump Dumpable
    deriving Show

data Task = Task
  { taskId      :: String
  , taskDesc    :: String
  , taskStatus  :: Bool
  , taskSubtasks :: [Task]
  } deriving Show

dump :: Dumpable -> IO ()
dump Examples = mapM_ print examples


examples :: [Command]
examples = [
    AddTask "Task1" "Wash the bathroom",
    AddSubtask "1.1" "Wash the sink" "Task1",
    AddSubtask "1.1.1" "Clean the faucet" "1.1",
    AddSubtask "1.1.2" "Clean the floor" "1.1",
    RemoveTask "1.1.2",
    CompleteTask "1.1" True,         -- recursively true
    ListTasks Nothing,
    Dump Examples
    ]
