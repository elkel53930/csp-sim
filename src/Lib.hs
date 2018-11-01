module Lib
    ( someFunc
    ) where

someFunc :: IO ()
someFunc = do
  runProcess sample

type Event = String

data Process =
    Prefix Event Process
  | ExtCh Process Process
  | Stop

sample :: Process
sample = ExtCh (Prefix "A" . Prefix "B" $ Stop) (Prefix "C" $ Stop)

runProcess :: Process -> IO()
runProcess Stop = putStrLn "Stop"
runProcess p = do
  e <- getLine
  if isAcceptableEvent p e
    then do {putStrLn e; runProcess $ getNextProcess p e}
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
