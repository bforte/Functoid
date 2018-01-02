
module Eval (evalProg) where

import LambdaCalc
import Parser
import Types

import Control.Monad
import Control.Monad.IO.Class
import Data.Array
import System.Random hiding (next)
import Prelude hiding (exp,print)
import qualified Prelude as P

-- Currently used characters:
-- cBCIKoOSUWYTF][+-*`ZLlGg0123456789@<>^v?$~.,;#"


print :: Show a => a -> LC ()
print =  liftIO . P.print

putMaybe :: Pretty a => Maybe a -> LC ()
putMaybe (Just a) = liftIO . P.putStr $ pretty a
putMaybe Nothing  = return ()


-- Evaluate a Lambdoid program
evalProg :: Bool -> [String] -> String -> IO () --Env
evalProg verbose args' src = P.print =<< runProg initState go -- TODO: parse args'

  where initState = Env (parse src) [] (Lam $ Var 1) (0,0) R

        parse str = array ((0,0),(m,n)) $ concat
          [ [((i,j),c) | (j,c) <- enum line] | (i,line) <- zip [0..] rows ]
          where rows = lines str
                enum x = zip [0..n] $ x ++ repeat ' '
                m = length rows - 1
                n = maximum (length <$> rows) - 1

        go = getCmd >>= \case '@' -> return ()
                              c   -> evalCmd c >> step >> go


evalCmd :: Char -> LC ()
evalCmd c = case c of
  -- Modify the direction pointer
  '<' -> dir .= L
  '>' -> dir .= R
  '^' -> dir .= U
  'v' -> dir .= D
  '?' -> liftIO (toEnum <$> randomRIO (0, 3)) >>= (dir .=)
  '$' -> -- Use one command-line argument
    use args >>= \case
      (a:as) -> args .= as >> appExp a
      []     -> appExp (number 0)
  '~' -> -- Ask user for input & apply it
    parseInput <$> liftIO getLine >>= \case
      Left _  -> return ()
      Right e -> appExp e
  ';' -> -- Output value as Bool
    toBool <$> use exp >>= putMaybe
  ',' -> -- Output value as ASCII char
    toChar <$> use exp >>= putMaybe
  '.' -> -- Output value as Int
    toIntegr <$> use exp >>= putMaybe
  '#' -> -- Jump instruction
    step
  'c' -> -- Replace current expression with id
    exp .= Lam (Var 1)
  '"' -> -- Accumulate number & apply it
    accumNumber >>= appExp
  _ | c `elem` "0123456789" -> -- Apply a number literal
        appExp . number $ toNumber c
    | c `elem` map fst opTable -> -- Apply an expression from opTable
        appExp $ fromChar c
    | otherwise -> -- No-op
        return ()

  where appExp :: Exp -> LC ()
        appExp e = exp %= simplify . (.$ e)

        fromChar :: Char -> Exp
        fromChar c
          | c `elem` map fst opTable = head [ op | (c',op) <- opTable, c' == c]
          | otherwise = error $ "command " ++ show c ++ " not implemented"

        accumNumber :: LC Exp
        accumNumber = number . foldl ((+).(10*)) 0 <$> go
          where go = getCmd >>= \case
                       '"' -> return []
                       c   -> step >> (toNumber c :) <$> go

        toNumber :: Char -> Integer
        toNumber c
          | c `elem` "0123456789" = fromIntegral $ fromEnum c - 48
          | otherwise = fromIntegral $ fromEnum c


getCmd :: LC Char
getCmd = do
  e' <- get
  print $ show (e' ^. pos) ++ " [" ++ show (e' ^. dir) ++ "]"
  return $ (e' ^. prog) ! (e' ^. pos)


step :: LC ()
step = do e  <- get
          pos %= next (snd . bounds $ e ^. prog) (dxdy $ e ^. dir)

  where next (m,n) (dx,dy) (x,y) = (mod(x+dx)(m+1), mod(y+dy)(n+1))

        dxdy L = ( 0,-1)
        dxdy R = ( 0, 1)
        dxdy U = (-1, 0)
        dxdy D = ( 1, 0)
