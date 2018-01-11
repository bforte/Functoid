
module CombinatoryLogic (Exp(..), toCL) where

-- | Redefine Exp to exclude literals or triples and allow SKIBCW
data Exp = Var Integer
         | Lam Exp
         | App Exp Exp
         | S |    K | I
         | B | C    | W
         | Func Integer
  deriving Eq

(.$) = App

instance Show Exp where
  show (Var a) = "x" ++ show a
  show (Lam a@(App _ _)) = "λ(" ++ show a ++ ")"
  show (Lam a) = "λ" ++ show a
  show (App a b@(App _ _)) = show a ++ " (" ++ show b ++ ")"
  show (App a b) = show a ++ " " ++ show b
  show S = "S"
  show K = "K"
  show I = "I"
  show B = "B"
  show C = "C"
  show W = "W"
  show (Func s) = "F" ++ show s


-- | Transform expression to combinatory logic & make sure onlno variables are left
toCL :: Exp -> Either String Exp
toCL = verify . go
  where go l@(Lam a)
          | notFree a = K .$ go (unLambda a)
          | otherwise   = case a of
              Var 1 -> I
              Var _ -> error "won't get here"
              Lam b -> go . Lam . go $ Lam  b
              App b c | free b && free c -> S .$ go (Lam b) .$ go (Lam c)
                      | free b           -> C .$ go (Lam b) .$ go (unLambda c)
                      | otherwise        -> B .$ go (unLambda b) .$ go (Lam c)
              _ -> l
        go (App a b) = go a .$ go b
        go a = a

        verify v@(Var _) = Left $ "Found variable '" ++ show v ++ "'"
        verify l@(Lam _) = Left $ "Found lambda '" ++ show l ++ "'"
        verify (App a b) = App <$> verify a <*> verify b
        verify a = Right a


-- | Check if (Var 1) is free in expression
free :: Exp -> Bool
free = not . notFree

-- | Check if (Var 1) is not free in expression
notFree :: Exp -> Bool
notFree = go 1
  where go n (Var i) = i /= n
        go n (Lam a) = go (n+1) a
        go n (App a b) = go n a && go n b
        go _ _ = True

-- | Decrement indices of variables referring to outer lambdas
unLambda :: Exp -> Exp
unLambda = go 1
  where go n v@(Var i)
          | n <= i    = Var (i-1)
          | otherwise = v
        go n (Lam a)  = Lam (go (n+1) a)
        go n (App a b)= App (go n a) (go n b)
        go _ x = x
