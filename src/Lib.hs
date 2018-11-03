module Lib (
    step
  , runProcess
  , someFunc
  ) where

import Type
import System.Random
import Control.Monad

someFunc :: IO ()
someFunc = runProcess $ Sequential (ExtCh (Prefix (Event "C") Stop) (Prefix (Event "B") (Prefix (Event "A") Skip))) (Prefix (Event "B") Stop)

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
runProcess p = do
  p' <- loop runAutoEvent p
  putStrLn $ show p'
  putStrLn "Choose event > "
  e <- getLine
  if isAcceptableEvent p' (Event e)
    then do {putStrLn (e ++ " is accepted."); p'' <- getNextProcess p' (Event e); runProcess p''}
    else do {putStrLn "Cannot accept."; runProcess $ p'}

loop :: (Eq a) => (a -> IO a) -> a -> IO a
loop f a = do
  a' <- f a
  if a == a'
    then return a
    else loop f a'

runAutoEvent :: Process -> IO Process
runAutoEvent p
  | isAcceptableEvent p Tau = auto p Tau
  | isAcceptableEvent p Check = auto p Check
  | otherwise = return p
  where
    auto p e = do
      putStrLn $ show p
      putStrLn $ show e ++ " is accepted."
      p' <- getNextProcess p e
      return p'

isAcceptableEvent :: Process -> Event -> Bool
isAcceptableEvent Stop _ = False
isAcceptableEvent Skip e = e == Check
isAcceptableEvent (Prefix ev _) e = ev == e
isAcceptableEvent (ExtCh p q) e = isAcceptableEvent p e || isAcceptableEvent q e
isAcceptableEvent (IntCh _ _) e = e == Tau
isAcceptableEvent (If b p q) e = if execBooleanExpression b then isAcceptableEvent p e else isAcceptableEvent q e
isAcceptableEvent (Guard b p) e = isAcceptableEvent (If b p Stop) e
--isAcceptableEvent (Sequential p _) Check = isAcceptableEvent p Check
isAcceptableEvent (Sequential p _) e = isAcceptableEvent p e

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
getNextProcess (Sequential p q) Check = if isAcceptableEvent p Check
  then return q
  else error "Invalid event."
getNextProcess (Sequential p q) e = do{p'<-getNextProcess p e; return $ Sequential p' q}
getNextProcess Skip _ = error "Invalid event. skip"
getNextProcess Stop _ = error "Invalid event."

chooseRandom :: (a,a) -> IO a
chooseRandom (a1,a2) = do
  r <- randomIO :: IO Int
  if (r < 0)
    then return a1
    else return a2

step :: Process -> Event -> IO Process
step p e = do
  p' <- getNextProcess p e
  loop runAutoEvent p'
