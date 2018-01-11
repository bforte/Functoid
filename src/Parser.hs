
module Parser (parseSrc, parseInput, parseExp) where

import qualified CombinatoryLogic as CL
import LambdaCalc
import Types

import Control.Arrow
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

-- | Parse a lambda expression; allowing Church numerals and builtins
parseInput :: String -> Either String Exp
parseInput = left show . parse (mkParser App Lam Var ps) "input" . trim
  where ps = [number <$> intP, toExp <$> oneOf builtins]

-- | Parse a lambda expression; allowing function literals & SKIBCW combinators
parseExp :: String -> Either String CL.Exp
parseExp = left show . parse (mkParser CL.App CL.Lam CL.Var ps) "command-line" . trim
  where ps = (CL.Func <$> (char 'F' *> intP)) : map clP [CL.S,CL.K,CL.I,CL.B,CL.C,CL.W]
        clP c = char (head $ show c) *> return c :: Parser CL.Exp

-- | Generate an expression parser from constructors and atomic parsers
mkParser :: (a -> a -> a) -> (a -> a) -> (Integer -> a) -> [Parser a] -> Parser a
mkParser app lam var ps = expP <* eof
  where expP  = spaced $ buildExpressionParser [[Infix appP AssocLeft]] atomP
        atomP =  varP
             <|> lamP
             <|> choice ps
             <|> parens expP
        appP = many1 space *> return app
        varP = var <$> (char 'x' *> intP)
        lamP = lam <$> (oneOf "λ\\" *> atomP)


trim :: String -> String
trim = dropWhile (==' ') . dropWhileEnd (==' ')

intP :: Parser Integer
intP = read <$> many1 digit

parens :: Parser a -> Parser a
parens p = between (char '(') (char ')') (spaced p)

spaced :: Parser a -> Parser a
spaced p = spaces *> p <* spaces
