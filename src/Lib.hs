module Lib where

import Type
import System.Random
import Control.Monad

someFunc :: IO ()
someFunc = undefined

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

runProcess :: Process -> IO ()
runProcess Stop = putStrLn $ show Stop
runProcess p
  | isAcceptableEvent p Tau = do
      putStrLn $ show p
      putStrLn $ show Tau
      p <- getNextProcess p Tau
      runProcess p
  | otherwise               = do
      putStrLn $ show p
      putStrLn "Choose event > "
      e <- getLine
      if isAcceptableEvent p (Event e)
        then do {putStrLn (e ++ " is accepted."); p <- getNextProcess p (Event e); runProcess p}
        else do {putStrLn "Cannot accept."; runProcess $ p}

isAcceptableEvent :: Process -> Event -> Bool
isAcceptableEvent Stop _ = False
isAcceptableEvent (Prefix ev _) e = ev == e
isAcceptableEvent (ExtCh p1 p2) e = isAcceptableEvent p1 e || isAcceptableEvent p2 e
isAcceptableEvent (IntCh _ _) e = e == Tau
isAcceptableEvent (If b p1 p2) e = if execBooleanExpression b then isAcceptableEvent p1 e else isAcceptableEvent p2 e
isAcceptableEvent (Guard b p) e = isAcceptableEvent (If b p Stop) e

getNextProcess :: Process -> Event -> IO Process
getNextProcess (Prefix ev pr) e = if ev == e then return pr else error "Invalid event."
getNextProcess (ExtCh p1 p2) Tau = case (isAcceptableEvent p1 Tau, isAcceptableEvent p2 Tau) of
    (True, False)  -> do{p<-getNextProcess p1 Tau; return (ExtCh p p2)}
    (False, True)  -> do{p<-getNextProcess p2 Tau; return (ExtCh p1 p)}
    (True, True)   -> do{p<-chooseRandom (p1,p2); getNextProcess p Tau}
    (False, False) -> error "Invalid event."
getNextProcess (ExtCh p1 p2) e = case (isAcceptableEvent p1 e, isAcceptableEvent p2 e) of
    (True, False)  -> getNextProcess p1 e
    (False, True)  -> getNextProcess p2 e
    (True, True)   -> do{p<-chooseRandom (p1,p2); getNextProcess p e}
    (False, False) -> error "Invalid event."
getNextProcess (IntCh p1 p2) e = if e == Tau
  then do{p<-chooseRandom (p1,p2); return p}
  else error "Invalid event."
getNextProcess (If b p1 p2) e = if execBooleanExpression b
  then getNextProcess p1 e
  else getNextProcess p2 e
getNextProcess (Guard b p) e = getNextProcess (If b p Stop) e

chooseRandom :: (a,a) -> IO a
chooseRandom (a1,a2) = do
  r <- randomIO :: IO Int
  if (r < 0)
    then return a1
    else return a2
