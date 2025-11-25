{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module Lib2(
    parseCommand
    , ToCliCommand(..)
    , process) where
import qualified Lib1

type ErrorMsg = String
type Parser a = String -> Either ErrorMsg (a, String)

-- user input
parseCommand :: Parser Lib1.Command
parseCommand = 
    orElse parseAddTask
    (orElse parseAddSubtask
    (orElse parseRemoveTask
    (orElse parseListTasks
    (orElse parseCompleteTask
    (orElse parseReportTasks
           parseDump)))))

process :: Lib1.Command -> [String]
process (Lib1.Dump Lib1.Examples) = "Examples:" : map toCliCommand Lib1.examples
process c = ["Parsed as " ++ show c]

class ToCliCommand a where
  toCliCommand :: a -> String

-- to CLI string conversion
instance ToCliCommand Lib1.Command where
  toCliCommand :: Lib1.Command -> String
  toCliCommand (Lib1.AddTask taskId desc) = 
    "add task " ++ taskId ++ " \"" ++ desc ++ "\""
  toCliCommand (Lib1.AddSubtask subtaskId desc parentId) = 
    "add subtask " ++ subtaskId ++ " \"" ++ desc ++ "\" to " ++ parentId
  toCliCommand (Lib1.RemoveTask taskId) = 
    "remove task " ++ taskId
  toCliCommand (Lib1.ListTasks Nothing) = 
    "list tasks"
  toCliCommand (Lib1.ListTasks (Just taskId)) = 
    "list tasks " ++ taskId
  toCliCommand (Lib1.CompleteTask taskId True) = 
    "complete task " ++ taskId ++ " recursively"
  toCliCommand (Lib1.CompleteTask taskId False) = 
    "complete task " ++ taskId
  toCliCommand Lib1.ReportTasks = 
    "report tasks"
  toCliCommand (Lib1.Dump Lib1.Examples) = 
    "dump examples"

-- manual Eq instance for Command
instance Eq Lib1.Command where
  (==) :: Lib1.Command -> Lib1.Command -> Bool
  (Lib1.AddTask id1 desc1) == (Lib1.AddTask id2 desc2) = 
    id1 == id2 && desc1 == desc2
  (Lib1.AddSubtask sid1 desc1 pid1) == (Lib1.AddSubtask sid2 desc2 pid2) = 
    sid1 == sid2 && desc1 == desc2 && pid1 == pid2
  (Lib1.RemoveTask id1) == (Lib1.RemoveTask id2) = 
    id1 == id2
  (Lib1.ListTasks m1) == (Lib1.ListTasks m2) = 
    m1 == m2
  (Lib1.CompleteTask id1 rec1) == (Lib1.CompleteTask id2 rec2) = 
    id1 == id2 && rec1 == rec2
  Lib1.ReportTasks == Lib1.ReportTasks = 
    True
  (Lib1.Dump d1) == (Lib1.Dump d2) = 
    d1 == d2
  _ == _ = False

instance Eq Lib1.Dumpable where
  Lib1.Examples == Lib1.Examples = True

-- Parser Combinators

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 input = case p1 input of
  Right result -> Right result
  Left _ -> p2 input

and2 :: Parser a -> Parser b -> Parser (a, b)
and2 p1 p2 input = case p1 input of
  Left err -> Left err
  Right (v1, rest1) -> case p2 rest1 of
    Left err -> Left err
    Right (v2, rest2) -> Right ((v1, v2), rest2)

and3 :: Parser a -> Parser b -> Parser c -> Parser (a, b, c)
and3 p1 p2 p3 input = case p1 input of
  Left err -> Left err
  Right (v1, rest1) -> case p2 rest1 of
    Left err -> Left err
    Right (v2, rest2) -> case p3 rest2 of
      Left err -> Left err
      Right (v3, rest3) -> Right ((v1, v2, v3), rest3)

and4 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser (a, b, c, d)
and4 p1 p2 p3 p4 input = case p1 input of
  Left err -> Left err
  Right (v1, rest1) -> case p2 rest1 of
    Left err -> Left err
    Right (v2, rest2) -> case p3 rest2 of
      Left err -> Left err
      Right (v3, rest3) -> case p4 rest3 of
        Left err -> Left err
        Right (v4, rest4) -> Right ((v1, v2, v3, v4), rest4)

and5 :: Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser (a, b, c, d, e)
and5 p1 p2 p3 p4 p5 input = case p1 input of
  Left err -> Left err
  Right (v1, rest1) -> case p2 rest1 of
    Left err -> Left err
    Right (v2, rest2) -> case p3 rest2 of
      Left err -> Left err
      Right (v3, rest3) -> case p4 rest3 of
        Left err -> Left err
        Right (v4, rest4) -> case p5 rest4 of
          Left err -> Left err
          Right (v5, rest5) -> Right ((v1, v2, v3, v4, v5), rest5)

-- Primitive Parsers

parseChar :: Parser Char
parseChar [] = Left "Expected character, got end of input"
parseChar (c:cs) = Right (c, cs)

parseDigit :: Parser Char
parseDigit input = case input of
  [] -> Left "Expected digit, got end of input"
  (c:cs) -> if c >= '0' && c <= '9'
            then Right (c, cs)
            else Left ("Expected digit, got '" ++ [c] ++ "'")

parseLetter :: Parser Char
parseLetter input = case input of
  [] -> Left "Expected letter, got end of input"
  (c:cs) -> if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
            then Right (c, cs)
            else Left ("Expected letter, got '" ++ [c] ++ "'")

parseWhitespace :: Parser String
parseWhitespace input = Right (span isWhitespace input)
  where
    isWhitespace c = c == ' ' || c == '\t' || c == '\n' || c == '\r'

parseWhitespace1 :: Parser String
parseWhitespace1 input = case parseWhitespace input of
  Right ("", _) -> Left "Expected whitespace"
  result -> result

parseString :: Parser String
parseString input = case parseChar input of
  Left err -> Left err
  Right ('"', rest) -> parseStringContent rest ""
  Right (c, _) -> Left ("Expected '\"', got '" ++ [c] ++ "'")
  where
    parseStringContent [] _ = Left "Unexpected end of string"
    parseStringContent ('"':rest) acc = Right (reverse acc, rest)
    parseStringContent ('\\':c:rest) acc = parseStringContent rest (c:acc)
    parseStringContent (c:rest) acc = parseStringContent rest (c:acc)

parseStringLiteral :: String -> Parser String
parseStringLiteral expected input = 
  if take (length expected) input == expected
  then Right (expected, drop (length expected) input)
  else Left ("Expected '" ++ expected ++ "'")

parseTaskId :: Parser String
parseTaskId input = case parseTaskIdContent input "" of
  ([], _) -> Left "Expected task ID"
  (taskId, rest) -> Right (taskId, rest)
  where
    parseTaskIdContent [] acc = (reverse acc, [])
    parseTaskIdContent (c:cs) acc = 
      if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || 
         (c >= '0' && c <= '9') || c == '.' || c == '_'
      then parseTaskIdContent cs (c:acc)
      else (reverse acc, c:cs)

-- Command Parsers

-- add task <taskId> <string>
parseAddTask :: Parser Lib1.Command
parseAddTask input = case and4 
  (parseStringLiteral "add task")
  parseWhitespace1
  parseTaskId
  (and2 parseWhitespace1 parseString)
  input of
    Right ((_, _, taskId, (_, desc)), rest) -> 
      Right (Lib1.AddTask taskId desc, rest)
    Left err -> Left err

-- add subtask <taskId> <string> to <taskId>
parseAddSubtask :: Parser Lib1.Command
parseAddSubtask input = case and5
  (parseStringLiteral "add subtask")
  (and2 parseWhitespace1 parseTaskId)
  (and2 parseWhitespace1 parseString)
  (and2 parseWhitespace1 (parseStringLiteral "to"))
  (and2 parseWhitespace1 parseTaskId)
  input of
    Right ((_, (_, subtaskId), (_, desc), (_, _), (_, parentId)), rest) ->
      Right (Lib1.AddSubtask subtaskId desc parentId, rest)
    Left err -> Left err

-- remove task <taskId>
parseRemoveTask :: Parser Lib1.Command
parseRemoveTask input = case and3
  (parseStringLiteral "remove task")
  parseWhitespace1
  parseTaskId
  input of
    Right ((_, _, taskId), rest) -> 
      Right (Lib1.RemoveTask taskId, rest)
    Left err -> Left err

-- list tasks [<taskId>]?
parseListTasks :: Parser Lib1.Command
parseListTasks input = case and2
  (parseStringLiteral "list tasks")
  parseWhitespace
  input of
    Right ((_, ws), rest) -> 
      if null ws || null rest
      then Right (Lib1.ListTasks Nothing, rest)
      else case parseTaskId rest of
        Right (taskId, rest2) -> Right (Lib1.ListTasks (Just taskId), rest2)
        Left _ -> Right (Lib1.ListTasks Nothing, rest)
    Left err -> Left err

-- complete task <taskId> [recursively]?
parseCompleteTask :: Parser Lib1.Command
parseCompleteTask input = case and3
  (parseStringLiteral "complete task")
  parseWhitespace1
  parseTaskId
  input of
    Right ((_, _, taskId), rest) -> 
      case parseWhitespace rest of
        Right (ws, rest2) ->
          if null ws || null rest2
          then Right (Lib1.CompleteTask taskId False, rest2)
          else case parseStringLiteral "recursively" rest2 of
            Right (_, rest3) -> Right (Lib1.CompleteTask taskId True, rest3)
            Left _ -> Right (Lib1.CompleteTask taskId False, rest2)
        Left _ -> Right (Lib1.CompleteTask taskId False, rest)
    Left err -> Left err

-- report tasks
parseReportTasks :: Parser Lib1.Command
parseReportTasks input = case parseStringLiteral "report tasks" input of
  Right (_, rest) -> Right (Lib1.ReportTasks, rest)
  Left err -> Left err

-- dump examples
parseDump :: Parser Lib1.Command
parseDump input = case parseStringLiteral "dump examples" input of
  Right (_, rest) -> Right (Lib1.Dump Lib1.Examples, rest)
  Left err -> Left err