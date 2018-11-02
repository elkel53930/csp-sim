module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  runProcess sample

data Event =
    Event String
--  | Tau
--  | Check -- 成功終了
  deriving (Eq)

data Process =
    Prefix Event Process
  | ExtCh Process Process
  | Stop
  deriving (Eq)

instance Show Event where
  show (Event e) = e
--  show Tau = "Tau"
--  show Check = "Check"

instance Show Process where
  show (Prefix e p) = show e ++ " -> " ++ show p
  show (ExtCh p1 p2) = "(" ++ show p1 ++ ") [] (" ++ show p2 ++ ")"
  show Stop = "Stop"

sample :: Process
sample = ExtCh (Prefix (Event "A") . Prefix (Event "B") $ Stop) (Prefix (Event "C") $ Stop)

runProcess :: Process -> IO ()
runProcess Stop = putStrLn "Stop"
runProcess p = do
  putStrLn $ show p
  e <- getLine
  if isAcceptableEvent p (Event e)
    then do {putStrLn e; runProcess $ getNextProcess p (Event e)}
    else do {putStrLn "Cannot accept."; runProcess $ p}

isAcceptableEvent :: Process -> Event -> Bool
isAcceptableEvent Stop _ = False
isAcceptableEvent (Prefix ev _) e = ev == e
isAcceptableEvent (ExtCh p1 p2) e = isAcceptableEvent p1 e || isAcceptableEvent p2 e

getNextProcess :: Process -> Event -> Process
getNextProcess (Prefix ev pr) e = if ev == e then pr else error "Invalid event."
getNextProcess (ExtCh p1 p2) e = case (isAcceptableEvent p1 e, isAcceptableEvent p2 e) of
    (True, False)  -> getNextProcess p1 e
    (False, True)  -> getNextProcess p2 e
    (True, True)   -> getNextProcess p1 e -- Todo ランダム
    (False, False) -> error "Invalid event."
