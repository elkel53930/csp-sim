import Lib

import Control.Monad

prExtCh = ExtCh (Prefix (Event "A") . Prefix (Event "B") $ Stop) (Prefix (Event "C") $ Stop)
evExtCh1 = [Event "A", Event "B"]
evExtCh2 =  [Event "C"]

tests :: [(Process, [Event])]
tests = [ (prExtCh, evExtCh1)
        , (prExtCh, evExtCh2)
        ]

doTest :: (Process, [Event]) -> IO Process
doTest (p, es) = foldM getNextProcess p es

main :: IO ()
main = do
  forM_ tests (\t -> do
    p <- doTest t
    putStrLn $ show p)
