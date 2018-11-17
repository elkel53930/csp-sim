module Parse where

import Type
import Text.ParserCombinators.Parsec
import Text.Parsec.Pos
import Text.Parsec.Char
import Data.Map

processes :: Parser Processes
processes = do
  sp
  ps <- many1 processExpression
  sp
  return $ fromList ps

processExpression :: Parser (Name, Process)
processExpression = do
  n <- processName
  sp
  char '='
  sp
  p <- process
  sp
  return (n,p)

process :: Parser Process
process = try prefix
      <|> try pName
      <|> try extCh
      <|> try intCh
      <|> try sequential
      <|> try skip
      <|> try stop

prefix :: Parser Process
prefix = do
  e <- event
  sp
  string "->"
  sp
  p <- process
  return $ Prefix e p

pName :: Parser Process
pName = do
  n <- processName
  return $ PName n

extCh :: Parser Process
extCh = do
  char '('
  sp
  p1 <- process
  sp
  string "[]"
  sp
  p2 <- process
  sp
  char ')'
  return $ ExtCh p1 p2

intCh :: Parser Process
intCh = do
  char '('
  sp
  p1 <- process
  sp
  string "|~|"
  sp
  p2 <- process
  sp
  char ')'
  return $ IntCh p1 p2

sequential :: Parser Process
sequential = do
  char '('
  sp
  p1 <- process
  sp
  string ";"
  sp
  p2 <- process
  sp
  char ')'
  return $ Sequential p1 p2

skip :: Parser Process
skip = do
  string "$Skip"
  return Skip

stop :: Parser Process
stop = do
  string "$Stop"
  return Stop

-- プロセス名は大文字始まり
processName :: Parser Name
processName = do
  h <- upper
  t <- many $ letter <|> char '_'
  return $ [h] ++ t

-- イベント名は小文字始まり
event :: Parser Event
event = do
  h <- lower
  t <- many $ letter <|> char '_'
  return . Event $ [h] ++ t

sp :: Parser String
sp = many space

sp1 :: Parser String
sp1 = many1 space
