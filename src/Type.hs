module Type where

data Event =
    Event String
  | Tau
--  | Check -- 成功終了
  deriving (Eq)

instance Show Event where
  show (Event e) = e
  show Tau = "Tau"
  --show Check = "Check"


data Process =
    Prefix Event Process
  | ExtCh Process Process
  | IntCh Process Process
  | If BooleanExpression Process Process
  | Guard BooleanExpression Process
  | Stop
  deriving (Eq)

instance Show Process where
  show (Prefix e p) = show e ++ " -> " ++  if isPrefix p then show p else "(" ++ show p ++ ")"
  show (ExtCh p1 p2) = "(" ++ show p1 ++ ") [] (" ++ show p2 ++ ")"
  show Stop = "Stop"
  show (IntCh p1 p2) = "(" ++ show p1 ++ ") |~| (" ++ show p2 ++ ")"
  show (If b p1 p2) = "if " ++ show b ++ " then " ++ show p1 ++ " else " ++ show p2
  show (Guard b p) = show b ++ "&(" ++ show p ++ ")"

data IntExpression =
    Num Int
  | Add IntExpression IntExpression
  | Sub IntExpression IntExpression
  | Mul IntExpression IntExpression
  | Div IntExpression IntExpression
  deriving (Eq)

instance Show IntExpression where
  show (Num n)     = show n
  show (Add n1 n2) = showExpression "+" n1 n2
  show (Sub n1 n2) = showExpression "-" n1 n2
  show (Mul n1 n2) = showExpression "*" n1 n2
  show (Div n1 n2) = showExpression "/" n1 n2

data BooleanExpression =
    Equal IntExpression IntExpression       -- (==)
  | GreaterThan IntExpression IntExpression -- (>)
  | LessThan IntExpression IntExpression    -- (<)
  deriving (Eq)

instance Show BooleanExpression where
  show (Equal n1 n2)       = showExpression "==" n1 n2
  show (GreaterThan n1 n2) = showExpression ">" n1 n2
  show (LessThan n1 n2)    = showExpression "<" n1 n2


showExpression :: Show a => String -> a -> a -> String
showExpression op n1 n2 = "(" ++ show n1 ++ op ++ show n2 ++ ")"

isPrefix :: Process -> Bool
isPrefix (Prefix _ _) = True
isPrefix _            = False
