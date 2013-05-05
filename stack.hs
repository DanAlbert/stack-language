module Stack where

type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP
         | INC
         | SWAP
         | POP Int
         | DEF String Prog
         | CALL String
         deriving Show

type Stack = [Int]
type Macros = [(String, Prog)]
type State = Maybe (Macros, Stack)

type Rank = Int
type CmdRank = (Int,Int)

rankC :: Cmd -> CmdRank
rankC (LD n)  = (0,1)
rankC ADD     = (2,1)
rankC MULT    = (2,1)
rankC DUP     = (1,2)
rankC INC     = (1,1)
rankC SWAP    = (2,2)
rankC (POP n) = (n,0)
rankC _       = (0,0) -- def and call do nothing

rank :: Prog -> Rank -> Maybe Rank
rank []     k             = Just k
rank (c:cs) k | k >= n    = rank cs r'
              | otherwise = Nothing
              where (n, m) = rankC c
                    r'     = k - n + m

rankP :: Prog -> Maybe Rank
rankP cs = rank cs 0

type D = State -> State

semCmd :: Cmd -> D
semCmd (LD n)    (Just (m, s))     = Just (m, n:s)
semCmd ADD       (Just (m, x:y:s)) = Just (m, x + y:s)
semCmd MULT      (Just (m, x:y:s)) = Just (m, x * y:s)
semCmd DUP       (Just (m, n:s))   = Just (m, n:n:s)
semCmd INC       (Just (m, n:s))   = Just (m, (n + 1):s)
semCmd SWAP      (Just (m, x:y:s)) = Just (m, y:x:s)
semCmd (POP n)   (Just (m, s))     = Just (m, drop n s)
semCmd (DEF n p) (Just (m, s))     = Just ((n, p):m, s)
semCmd (CALL n)  (Just (m, s))     = case lookup n m of
                                       Just prog -> semHelper prog (Just (m, s))
                                       _         -> Nothing
semCmd _         _                 = Nothing

semHelper :: Prog -> D
semHelper []     s = s
semHelper (c:cs) s = semHelper cs (semCmd c s)

sem :: Prog -> State
sem prog = semHelper prog (Just ([], []))
