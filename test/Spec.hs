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
evIf1 = [Event "C"]

tests :: [(Process, [Event])]
tests = [ (prExtCh, evExtCh1)
        , (prExtCh, evExtCh2)
        , (if1    , evIf1)
        ]

doTest :: (Process, [Event]) -> IO Process
doTest (p, es) = foldM getNextProcess p es

main :: IO ()
main = do
  forM_ tests (\t -> do
    p <- doTest t
    putStrLn $ show p)
