
module Parser where

import LambdaCalc

import Text.Parsec


type Parser = Parsec String ()

parseInput :: String -> Either ParseError Exp
parseInput = parse inputP "input"

inputP :: Parser Exp
inputP = intP <|> try charP -- <|> expP)
  where intP  = spaced (number . read <$> many1 digit) <* eof
        charP = number . fromIntegral . fromEnum <$> anyChar <* eof
        --expP  = parens expP' <|> expP'
        --expP' = varP <|> appP <|> lamP

        --varP = spaced $ Var . read <$> (char 'x' *> many1 digit)
        --lamP = do spaced $ choice (string <$> lam)
        --          Lam <$> spaced expP
        --appP = do a <- spaces *> expP
        --          many1 space
        --          b <- expP <* spaces
        --          return $ a .$ b

        --lam = ["\\","λ"]
        --to  = ["->","."]

        --parens :: Parser a -> Parser a
        --parens p = between (char '(') (char ')') (spaced p)

        spaced :: Parser a -> Parser a
        spaced p = spaces *> p <* spaces
