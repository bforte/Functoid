{-# LANGUAGE FlexibleInstances #-}

-- | Lambda terms based on DeBruijn notation; see here:
--    - https://en.wikipedia.org/wiki/De_Bruijn_index

module LambdaCalc
  ( Exp(..), Pretty, pretty, (.$)
  , simplify, number, opTable
  , toBool, toChar, toNum
  ) where

import Prelude hiding (pred,succ,and,or,not)


-- | Exp type for lambda calculus terms
data Exp = Var Integer
         | Lam Exp
         | Tri Exp Exp Exp  -- Triple (x,y,c) for modifying the source
         | App Exp Exp

(.$) = App

infixr 8 $.
($.) = App

λ = Lam

instance Show Exp where
  show (Var a) = "x" ++ show a
  show (Lam a@(App _ _)) = "λ(" ++ show a ++ ")"
  show (Lam a) = "λ" ++ show a
  show (Tri a b c) = "[" ++ show a ++ "," ++ show b ++ "," ++ show c ++ "]"
  show (App a b@(App _ _)) = show a ++ " (" ++ show b ++ ")"
  show (App a b) = show a ++ " " ++ show b

-- | Compare for equality by first simplifying both expressions
-- without getting lost in an infinite loop
instance Eq Exp where
  a == b = eq (simplify a) (simplify b)

eq (Var a) (Var b) = a == b
eq (Lam a) (Lam b) = eq a b
eq (Tri a b c) (Tri d e f) = eq a d && eq b e && eq c f
eq (App a b) (App c d) = eq a c && eq b d
eq _ _ = False


class Pretty a where
  pretty :: a -> String

instance Pretty Exp where
  pretty = show
instance Pretty Bool where
  pretty = show
instance Pretty Integer where
  pretty = show
instance Pretty Char where
  pretty c = [c]
instance Pretty String where
  pretty = id
instance Pretty a => Pretty (Maybe a) where
  pretty (Just a) = pretty a
  pretty Nothing  = ""


-- | Is the current expression a "boolean"
toBool :: Exp -> Maybe Bool
toBool x
  | x == true  = Just True
  | x == false = Just False
  | otherwise  = Nothing

-- | Pattern match Church numerals
toNum :: Exp -> Maybe Integer
toNum = toNum' . simplify
toNum' (Lam (Lam (Var 1))) = Just 0
toNum' (Lam (Lam (App (Var 2) x))) = go x
  where go (Var 1) = Just 1
        go (App (Var 2) x) = (1+) <$> go x
        go _ = Nothing
toNum' _ = Nothing

-- | Use the Church number (if any) & convert to ASCII
toChar :: Exp -> Maybe Char
toChar x = toEnum . (`mod` 128) . fromIntegral <$> toNum x


-- | Reduce Lambda terms in normal-order until the term doesn't simplify further
simplify :: Exp -> Exp
simplify e
  | eq e' e   = e
  | otherwise = simplify e'
  where e' = simplify' e
        simplify' (Lam a) = Lam $ simplify' a
        simplify' (Tri a b c) = Tri (simplify' a) (simplify' b) (simplify' c)
        simplify' (App (Lam a) b) = betaReduce a b
        simplify' (App a b) = App (simplify' a) (simplify' b)
        simplify' e = e

        betaReduce a b = sub 1 b a
          where sub n a (Var b)
                  | n == b = a
                  | n <  b = Var $ b-1
                  | otherwise = Var b
                sub n a (Lam b) = Lam $ sub (n+1) (incFree 0 a) b
                sub n a (Tri b c d) = Tri (sub n a b) (sub n a c) (sub n a d)
                sub n a (App b c) = App (sub n a b) (sub n a c)

                incFree n (Var a)
                  | n < a = Var $ a+1
                  | otherwise = Var a
                incFree n (Lam a) = Lam $ incFree (n+1) a
                incFree n (Tri a b c) = Tri (incFree n a) (incFree n b) (incFree n c)
                incFree n (App a b) = App (incFree n a) (incFree n b)

-- | Modify the source; arguments: @x y c@
-- where @(x,y)@ is the position (modulo source bounds) and
-- @c@ is the new character (modulo 128)
modify = Lam $ Lam $ Lam $ Tri (Var 3) (Var 2) (Var 1)


-- | Combinators
b  = Lam (Lam (Lam $ Var 3 .$ (Var 2 .$ Var 1)))
c  = Lam (Lam (Lam $ Var 3 .$ Var 1 .$ Var 2))
i  = Lam (Var 1)
k  = Lam (Lam $ Var 2)
_o  = Lam (Var 1 .$ Var 1)   -- ω
o = o .$ o                   -- Ω
s  = Lam (Lam (Lam $ Var 3 .$ Var 1 .$ (Var 2 .$ Var 1)))
u  = Lam (Lam (Var 1 .$ (Var 2 .$ Var 2 .$ Var 1)))
w  = Lam (Lam $ Var 2 .$ Var 1 .$ Var 1)
y  = Lam (Lam (Var 2 .$ (Var 1 .$ Var 1)) .$ Lam (Var 2 .$ (Var 1 .$ Var 1)))


-- | Boolean logic
true   = Lam (Lam $ Var 2)
false  = Lam (Lam $ Var 1)
ifelse = Lam (Lam (Lam $ Var 1 .$ Var 3 .$ Var 1))

and = Lam (Lam $ Var 2 .$ Var 1 .$ Var 2)
or  = Lam (Lam $ Var 2 .$ Var 2 .$ Var 1)
xor = Lam (Lam (Var 2 .$ (Var 1 .$ (Lam $ Lam $ Var 1) .$ (Lam $ Lam $ Var 2)) .$ Var 1))
not = Lam $ Var 1 .$ (Lam $ Lam $ Var 1) .$ (Lam $ Lam $ Var 2)


-- | Arithmetic (Church numerals)
number :: Integer -> Exp
number n = Lam (Lam (foldr App (Var 1) [Var 2 | _ <- [1..n]]))

succ = Lam (Lam (Lam (Var 2 .$ (Var 3 .$ Var 2 .$ Var 1))))
pred = Lam (Lam (Lam (Var 3 .$ Lam (Lam (Var 1 .$ (Var 2 .$ Var 4))) .$ Lam (Var 2) .$ Lam (Var 1))))
plus = Lam (Lam (Lam (Lam (Var 4 .$ Var 2 .$ (Var 3 .$ Var 2 .$ Var 1)))))
sub  = Lam (Lam (Var 1 .$ pred .$ Var 2))
mult = Lam (Lam (Lam (Var 3 .$ (Var 2 .$ Var 1))))
pow = Lam (Lam (Var 1 .$ Var 2))

iszero = Lam $ Var 1 .$ Lam (Lam (Lam $ Var 1)) .$ Lam (Lam $ Var 2)
eqq = simplify $ Lam (Lam $ and .$ (geq .$ Var 1 .$ Var 2) .$ (leq .$ Var 1 .$ Var 2))
leq = Lam (Lam (App (App (App (App (Var 1) (Lam (Lam (Lam (App (App (App (Var 3) (Lam (Lam (App (Var 1) (App (Var 2) (Var 4)))))) (Lam (Var 2))) (Lam (Var 1))))))) (Var 2)) (Lam (Lam (Lam (Var 1))))) (Lam (Lam (Var 2)))))
le = Lam (Lam (App (App (App (App (Var 1) (Lam (Lam (Lam (App (App (App (Var 3) (Lam (Lam (App (Var 1) (App (Var 2) (Var 4)))))) (Lam (Var 2))) (Lam (Var 1))))))) (Lam (Lam (App (Var 2) (App (App (Var 4) (Var 2)) (Var 1)))))) (Lam (Lam (Lam (Var 1))))) (Lam (Lam (Var 2)))))
geq = Lam (Lam (App (App (App (App (Var 2) (Lam (Lam (Lam (App (App (App (Var 3) (Lam (Lam (App (Var 1) (App (Var 2) (Var 4)))))) (Lam (Var 2))) (Lam (Var 1))))))) (Var 1)) (Lam (Lam (Lam (Var 1))))) (Lam (Lam (Var 2)))))
ge = Lam (Lam (App (App (App (App (Var 2) (Lam (Lam (Lam (App (App (App (Var 3) (Lam (Lam (App (Var 1) (App (Var 2) (Var 4)))))) (Lam (Var 2))) (Lam (Var 1))))))) (Lam (Lam (App (Var 2) (App (App (Var 3) (Var 2)) (Var 1)))))) (Lam (Lam (Lam (Var 1))))) (Lam (Lam (Var 2)))))


-- | Used to convert from a character in the source to Exps
opTable :: [(Char,Exp)]
opTable =
  [ ('B', b), ('C', c), ('I', i), ('K', k), ('o', _o), ('O', o), ('S', s), ('U', u), ('W', w), ('Y', y)
  , ('T', true), ('F', false), ('i', ifelse), ('n',not), ('A',and), ('V',or), ('X',xor)
  , (']', succ), ('[', pred)
  , ('+', plus), ('-', sub), ('*', mult), ('`', pow)
  , ('=', eqq), ('L', leq), ('l', le), ('G', geq), ('g', ge), ('Z', iszero)
  , ('%', modify)
  ]
