module Stack where

type Prog = [Cmd]

data Cmd = LD Int
         | ADD
         | MULT
         | DUP

type Stack = [Int]
type StackErr = Maybe Stack
type D = StackErr -> StackErr

semCmd :: Cmd -> D
semCmd (LD n) (Just stack)                = Just (n:stack)
semCmd ADD    (Just (first:second:stack)) = Just (first + second:stack)
semCmd MULT   (Just (first:second:stack)) = Just (first * second:stack)
semCmd DUP    (Just (n:stack))            = Just (n:n:stack)
semCmd _      _                           = Nothing

semHelper :: Prog -> D
semHelper [] s     = s
semHelper (c:cs) s = semHelper cs (semCmd c s)

sem :: Prog -> StackErr
sem prog = semHelper prog (Just [])
