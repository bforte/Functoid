
module Parser (parseSrc, parseInput) where

import LambdaCalc
import Types

import Data.Array
import Data.List
import Text.Parsec
import Text.Parsec.Expr


-- | Transform the source to an Array (pad shorter rows with spaces)
parseSrc :: String -> Prog
parseSrc str = array ((0,0),(m,n)) $ concat
  [ [((i,j),c) | (j,c) <- enum line] | (i,line) <- zip [0..] rows ]
  where rows = lines str
        enum x = zip [0..n] $ x ++ repeat ' '
        m = length rows - 1
        n = maximum (length <$> rows) - 1


type Parser = Parsec String ()

-- | Parse a string to lambda expression
parseInput :: String -> Either ParseError Exp
parseInput = parse inputP "input" . trim
  where trim = dropWhile (==' ') . dropWhileEnd (==' ')

-- | Parse DeBuijn style lambda expressions; numbers are treated as Church numerals
inputP :: Parser Exp
inputP = expP <* eof
  where expP  = spaced $ buildExpressionParser [[Infix appP AssocLeft]] atomP
        atomP =  varP
             <|> lamP
             <|> numP
             <|> builtinP
             <|> parens expP
        appP  = many1 space *> return App

        varP = Var <$> (char 'x' *> intP)
        lamP = Lam <$> (oneOf "λ\\" *> atomP)
        numP = number <$> intP
        builtinP = toExp <$> oneOf builtins


intP :: Parser Integer
intP = read <$> many1 digit

parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') (spaced p)

spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces
