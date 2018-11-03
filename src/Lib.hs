module Lib where

import System.Random
import Control.Monad

sample = Prefix (Event "I") (ExtCh (Prefix (Event "C") Stop) (IntCh (Prefix (Event "A") Stop) (Prefix (Event "B") Stop)))

someFunc :: IO ()
someFunc = runProcess sample

data Event =
    Event String
  | Tau
--  | Check -- 成功終了
  deriving (Eq)

data Process =
    Prefix Event Process
  | ExtCh Process Process
  | IntCh Process Process
  | Stop
  deriving (Eq)

instance Show Event where
  show (Event e) = e
  show Tau = "Tau"
--  show Check = "Check"

isPrefix :: Process -> Bool
isPrefix (Prefix _ _) = True
isPrefix _            = False

instance Show Process where
  show (Prefix e p) = show e ++ " -> " ++  if isPrefix p then show p else "(" ++ show p ++ ")"
  show (ExtCh p1 p2) = "(" ++ show p1 ++ ") [] (" ++ show p2 ++ ")"
  show Stop = "Stop"
  show (IntCh p1 p2) = "(" ++ show p1 ++ ") |~| (" ++ show p2 ++ ")"

runProcess :: Process -> IO ()
runProcess Stop = putStrLn $ show Stop
runProcess p
  | isAcceptableEvent p Tau = do {putStrLn $ show Tau; p <- getNextProcess p Tau; runProcess p}
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

chooseRandom :: (a,a) -> IO a
chooseRandom (a1,a2) = do
  r <- randomIO :: IO Int
  if (r < 0)
    then return a1
    else return a2
