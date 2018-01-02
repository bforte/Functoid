
-- Lambda terms based on DeBruijn notation (https://en.wikipedia.org/wiki/De_Bruijn_index)

module LambdaCalc
  ( Exp(..), Pretty, pretty, (.$)
  , simplify, number, opTable
  , toBool, toChar, toIntegr
  ) where

import Prelude hiding (pred,succ)


data Exp = Var Integer
         | Lam Exp
         | App Exp Exp

(.$) = App

instance Show Exp where
  show (Var a) = "x" ++ show a
  show (Lam a) = "λ" ++ show a
  show (App a b@(Var _)) = "(" ++ show a ++ " " ++ show b ++ ")"
  show (App a b) = "(" ++ show a ++ " (" ++ show b ++ "))"

instance Eq Exp where
  a == b = eq (simplify a) (simplify b)

eq (Var a) (Var b) = a == b
eq (Lam a) (Lam b) = eq a b
eq (App a b) (App c d) = eq a c && eq b d
eq _ _ = False


class Pretty a where
  pretty :: a -> String

instance Pretty Bool where
  pretty = show
instance Pretty Integer where
  pretty = show
instance Pretty Char where
  pretty c = [c]


toBool :: Exp -> Maybe Bool
toBool x
  | x == true  = Just True
  | x == false = Just False
  | otherwise  = Nothing

toIntegr :: Exp -> Maybe Integer
toIntegr (Lam (Lam (Var 1))) = Just 0
toIntegr (Lam (Lam (App (Var 2) x))) = go x
  where go (Var 1) = Just 1
        go (App (Var 2) x) = (1+) <$> go x
        go _ = Nothing
toIntegr _ = Nothing

toChar :: Exp -> Maybe Char
toChar x = toEnum . (`mod` 128) . fromIntegral <$> toIntegr x


-- Reduce Lambda terms in normal-order until the term doesn't simplify further
simplify :: Exp -> Exp
simplify e
  | eq e' e   = e
  | otherwise = simplify e'
  where e' = simplify' e
        simplify' (Lam a) = Lam $ simplify' a
        simplify' (App (Lam a) b) = betaReduce a b
        simplify' (App a b) = App (simplify' a) (simplify' b)
        simplify' e = e

betaReduce :: Exp -> Exp -> Exp
betaReduce a b = sub 1 b a
  where sub n a (Var b)
          | n == b = a
          | n <  b = Var $ b-1
          | otherwise = Var b
        sub n a (Lam b) = Lam $ sub (n+1) (incFree 0 a) b
        sub n a (App b c) = App (sub n a b) (sub n a c)

        incFree n (Var a)
          | n < a = Var $ a+1
          | otherwise = Var a
        incFree n (Lam a) = Lam $ incFree (n+1) a
        incFree n (App a b) = App (incFree n a) (incFree n b)


-- Combinators
b  = Lam (Lam (Lam $ Var 3 .$ (Var 2 .$ Var 1)))
c  = Lam (Lam (Lam $ Var 3 .$ Var 1 .$ Var 2))
i  = Lam (Var 1)
k  = Lam (Lam $ Var 2)
_o  = Lam (Var 1 .$ Var 1)
o = o .$ o
s  = Lam (Lam (Lam $ Var 3 .$ Var 1 .$ (Var 2 .$ Var 1)))
u  = Lam (Lam (Var 1 .$ (Var 2 .$ Var 2 .$ Var 1)))
w  = Lam (Lam $ Var 2 .$ Var 1 .$ Var 1)
y  = Lam (Lam (Var 2 .$ (Var 1 .$ Var 1)) .$ Lam (Var 2 .$ (Var 1 .$ Var 1)))

tests = and
  [ b == (s .$ (k .$ s)) .$ k
  , c == s .$ (s .$ (k .$ (s .$ (k .$ s) .$ k)) .$ s) .$ (k .$ k)
  , w == s .$ s .$ (s .$ k)
  , i == w .$ k
  , s == b .$ (b .$ (b .$ w) .$ c) .$ (b .$ b)
  , s == b .$ (b .$ w) .$ (b .$ b .$ c)
  ]

-- Boolean logic
true   = k
false  = s .$ k

-- Arithmetic (Church numerals)
number :: Integer -> Exp
number n = Lam (Lam (foldr App (Var 1) [Var 2 | _ <- [1..n]]))

succ = Lam (Lam (Lam (Var 2 .$ (Var 3 .$ Var 2 .$ Var 1))))
pred = Lam (Lam (Lam (Var 3 .$ (Lam (Lam (Var 1 .$ (Var 2 .$ Var 4)))) .$ Lam (Var 2) .$ Lam (Var 1))))
plus = Lam (Lam (Lam (Lam (Var 4 .$ Var 2 .$ (Var 3 .$ Var 2 .$ Var 1)))))
sub  = Lam (Lam (Var 1 .$ pred .$ Var 2))
mult = Lam (Lam (Lam (Var 3 .$ (Var 2 .$ Var 1))))
pow  = Lam (Lam (Var 1 .$ Var 2)) -- TODO: this definition only works for m,n>0
zero = Lam (Var 1 .$ Lam false .$ true)
leq  = Lam (Lam (zero .$ (sub .$ Var 2 .$ Var 1)))
le   = Lam (Lam (zero .$ (sub .$ (succ .$ Var 2) .$ Var 1)))
geq  = Lam (Lam (zero .$ (sub .$ Var 1 .$ Var 2)))
ge   = Lam (Lam (zero .$ (sub .$ (succ .$ Var 1) .$ Var 2)))

opTable =
  [ ('B', b), ('C', c), ('I', i), ('K', k), ('o', _o), ('O', o), ('S', s), ('U', u), ('W', w), ('Y', y)
  , ('T', true), ('F', false)
  , (']', succ), ('[', pred)
  , ('+', plus), ('-', sub), ('*', mult), ('`', pow)
  , ('Z', zero), ('L', leq), ('l', le), ('G', geq), ('g', ge)
  ]
