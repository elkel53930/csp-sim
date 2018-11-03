import Lib
import Type

import Control.Monad

prExtCh = ExtCh (Prefix (Event "A") . Prefix (Event "B") $ Stop) (Prefix (Event "C") $ Stop)
evExtCh1 = [Event "A", Event "B"]
evExtCh2 =  [Event "C"]

prefixA = Prefix (Event "A") Stop
prefixB = Prefix (Event "B") Stop
prefixC = Prefix (Event "C") Stop
intCh1 = IntCh prefixA prefixB
extCh1 = ExtCh prefixC intCh1
if1    = If (Equal (Num 1) (Add (Num 1) (Num 1))) prefixA extCh1
evIf1  = [Event "C"]

guard1 = Prefix (Event "G") $ Guard (Equal (Num 1) (Add (Num 1) (Num 1))) prefixA
guard2 = Guard (Equal (Num 1) (Num 1)) prefixB
extCh2 = ExtCh guard1 guard2

skip1  = Sequential (Prefix (Event "B") (Prefix (Event "A") Skip)) (prefixB)
skip2  = Sequential (ExtCh (Prefix (Event "C") Stop) (Prefix (Event "B") (Prefix (Event "A") Skip))) (prefixB)

tests :: [(Process, [Event])]
tests = [ (prExtCh, evExtCh1)
        , (prExtCh, evExtCh2)
        , (if1    , evIf1)
        , (guard1 , [Event "G"])
        , (guard2 , [Event "B"])
        , (extCh2 , [Event "B"])
        , (skip1  , [Event "B", Event "A", Event "B"])
        , (skip2  , [Event "C"])
        ]

doTest :: (Process, [Event]) -> IO Process
doTest (p, es) = foldM step p es

main :: IO ()
main = do
  forM_ tests (\t -> do
    putStrLn $ show t
    p <- doTest t
    putStrLn $ show p)
