module Lib (
    step
  , runProcess
  , someFunc
  ) where

import Type
import System.Random
import Control.Monad
import Data.Map

ps = fromList [("Test", Sequential (ExtCh (Prefix (Event "C") Stop) (Prefix (Event "B") (Prefix (Event "A") Skip))) (Prefix (Event "B") Stop))]

someFunc :: IO ()
someFunc = runProcess ps $ PName "Test"

execBooleanExpression :: BooleanExpression -> Bool
execBooleanExpression (Equal n1 n2) = execIntExpression n1 == execIntExpression n2
execBooleanExpression (GreaterThan n1 n2) = execIntExpression n1 > execIntExpression n2
execBooleanExpression (LessThan n1 n2) = execIntExpression n1 < execIntExpression n2

execIntExpression :: IntExpression -> Int
execIntExpression (Num n) = n
execIntExpression (Add n1 n2) = execIntExpression n1 + execIntExpression n2
execIntExpression (Sub n1 n2) = execIntExpression n1 - execIntExpression n2
execIntExpression (Mul n1 n2) = execIntExpression n1 * execIntExpression n2
execIntExpression (Div n1 n2) = execIntExpression n1 `div` execIntExpression n2

showProcess :: Processes -> Process -> String
showProcess ps (PName n) = showProcess ps (ps ! n)
showProcess ps p = show p

runProcess :: Processes -> Process -> IO ()
runProcess _ Stop = putStrLn $ show Stop
runProcess ps p = do
  p' <- loop (runAutoEvent ps) p
  putStrLn $ showProcess ps p'
  putStrLn "Choose event > "
  e <- getLine
  if isAcceptableEvent ps p' (Event e)
    then do {putStrLn (e ++ " is accepted."); p'' <- getNextProcess ps p' (Event e); runProcess ps p''}
    else do {putStrLn "Cannot accept."; runProcess ps p'}

loop :: (Eq a) => (a -> IO a) -> a -> IO a
loop f a = do
  a' <- f a
  if a == a'
    then return a
    else loop f a'

runAutoEvent :: Processes -> Process -> IO Process
runAutoEvent ps p
  | isAcceptableEvent ps p Tau = auto p Tau
  | isAcceptableEvent ps p Check = auto p Check
  | otherwise = return p
  where
    auto p e = do
      putStrLn $ showProcess ps p
      putStrLn $ show e ++ " is accepted."
      p' <- getNextProcess ps p e
      return p'

isAcceptableEvent :: Processes -> Process -> Event -> Bool
isAcceptableEvent _ Stop _ = False
isAcceptableEvent _ Skip e = e == Check
isAcceptableEvent _ (Prefix ev _) e = ev == e
isAcceptableEvent ps (ExtCh p q) e = isAcceptableEvent ps p e || isAcceptableEvent ps q e
isAcceptableEvent _ (IntCh _ _) e = e == Tau
isAcceptableEvent ps (If b p q) e = if execBooleanExpression b then isAcceptableEvent ps p e else isAcceptableEvent ps q e
isAcceptableEvent ps (Guard b p) e = isAcceptableEvent ps (If b p Stop) e
isAcceptableEvent ps (Sequential p _) e = isAcceptableEvent ps p e
isAcceptableEvent ps (PName n) e = isAcceptableEvent ps (ps ! n) e

getNextProcess :: Processes -> Process -> Event -> IO Process
getNextProcess _ (Prefix ev pr) e = if ev == e then return pr else error "Invalid event."
getNextProcess ps (PName n) e = getNextProcess ps (ps ! n) e
getNextProcess ps (ExtCh p1 p2) Tau = case (isAcceptableEvent ps p1 Tau, isAcceptableEvent ps p2 Tau) of
    (True, False)  -> do{p<-getNextProcess ps p1 Tau; return (ExtCh p p2)}
    (False, True)  -> do{p<-getNextProcess ps p2 Tau; return (ExtCh p1 p)}
    (True, True)   -> do{p<-chooseRandom (p1,p2); getNextProcess ps p Tau}
    (False, False) -> error "Invalid event."
getNextProcess ps (ExtCh p1 p2) e = case (isAcceptableEvent ps p1 e, isAcceptableEvent ps p2 e) of
    (True, False)  -> getNextProcess ps p1 e
    (False, True)  -> getNextProcess ps p2 e
    (True, True)   -> do{p<-chooseRandom (p1,p2); getNextProcess ps p e}
    (False, False) -> error "Invalid event."
getNextProcess _ (IntCh p1 p2) e = if e == Tau
  then do{p<-chooseRandom (p1,p2); return p}
  else error "Invalid event."
getNextProcess ps (If b p1 p2) e = if execBooleanExpression b
  then getNextProcess ps p1 e
  else getNextProcess ps p2 e
getNextProcess ps (Guard b p) e = getNextProcess ps (If b p Stop) e
getNextProcess ps (Sequential p q) Check = if isAcceptableEvent ps p Check
  then return q
  else error "Invalid event."
getNextProcess ps (Sequential p q) e = do{p'<-getNextProcess ps p e; return $ Sequential p' q}
getNextProcess _ Skip _ = error "Invalid event. State is Skip."
getNextProcess _ Stop _ = error "Invalid event. State is Stop."

chooseRandom :: (a,a) -> IO a
chooseRandom (a1,a2) = do
  r <- randomIO :: IO Int
  if (r < 0)
    then return a1
    else return a2

step :: Processes -> Process -> Event -> IO Process
step ps p e = do
  p' <- getNextProcess ps p e
  loop (runAutoEvent ps) p'
