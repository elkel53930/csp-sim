import Lib

import Control.Monad

prExtCh = ExtCh (Prefix (Event "A") . Prefix (Event "B") $ Stop) (Prefix (Event "C") $ Stop)
evExtCh1 = [Event "A", Event "B"]
evExtCh2 =  [Event "C"]
evExtCh3 =  [Event "D"]

tests :: [(Process, [Event])]
tests = [ (prExtCh, evExtCh1)
        , (prExtCh, evExtCh2)
        , (prExtCh, evExtCh3)
        ]

doTest :: (Process, [Event]) -> Process
doTest (p, es) = foldl getNextProcess p es

main :: IO ()
main = do
  forM_ tests (\t -> print $ doTest t)
