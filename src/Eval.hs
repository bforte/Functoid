
module Eval (evalProg) where

import LambdaCalc
import Parser
import Types

import Control.Monad
import Control.Monad.IO.Class
import Data.Array
import Data.List
import Data.Maybe
import Prelude hiding (exp,id)
import qualified Prelude as P
import System.IO
import System.Random hiding (next)

-- Currently used characters:
-- "#$*+,-.0123456789:;<>?@BCFGIKLOSTUWY[]^`cglov~


-- | Evaluate a Lambdoid program
evalProg :: Flags -> [Exp] -> String -> IO ()
evalProg fs as src = do
  out <- (^. exp) <$> execProg initState fs go

  let showNum  = fmap (("Church numeral: "++) . show) . toIntegr
      showBool = fmap (("Boolean: "++) . show) . toBool
      vals = case catMaybes $ map ($out) [showNum, showBool] of
               [] -> ""
               vs -> "\t[" ++ intercalate "; " vs ++ "]"

  when (not $ fs ^. quiet) $
    hPutStrLn stderr $ "\nFinal expression: " ++ show out ++ vals

  where initState = Env (parseSrc src) as id (0,0) R

        -- Loop the program until '@' is reached
        go = getCmd >>= \case '@' -> return ()
                              c   -> do evalCmd c
                                        exp %= simplify
                                        step >> go


-- | Decide what to do depending on the just read character
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
  ':' -> -- Output the current lambda term
    Just <$> use exp >>= putMaybe
  ';' -> -- Output value as Bool
    toBool <$> use exp >>= putMaybe
  ',' -> -- Output value as ASCII char
    toChar <$> use exp >>= putMaybe
  '.' -> -- Output value as Int
    toIntegr <$> use exp >>= putMaybe
  '#' -> -- Jump instruction
    step
  'c' -> -- Replace current expression with id
    exp .= id
  'l' -> -- Print newline
    putMaybe $ Just "\n"
  '"' -> -- Accumulate number & apply it
    step >> accumNumber >>= appExp
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
        accumNumber = number . foldl ((+).(10*)) 0 . filter (>0) <$> go
          where go = getCmd >>= \case
                       '"'                  -> return []
                       c | c `elem` "<>^v@" -> evalCmd c >> (-1:) <$> go
                         | otherwise        -> step >> (toNumber c :) <$> go

        toNumber :: Char -> Integer
        toNumber c
          | c `elem` "0123456789" = fromIntegral $ fromEnum c - 48
          | otherwise = fromIntegral $ fromEnum c


-- | Get the current command that gets executed
getCmd :: LC Char
getCmd = do
  env <- get
  whenM (view verbose) $
    putErrLn $ show (env ^. pos) ++ " [" ++ show (env ^. dir) ++ "]"
  return $ (env ^. prog) ! (env ^. pos)


-- | Step the program; move command pointer in the current direction
step :: LC ()
step = do e  <- get
          pos %= next (snd . bounds $ e ^. prog) (dxdy $ e ^. dir)

  where next (m,n) (dx,dy) (x,y) = (mod(x+dx)(m+1), mod(y+dy)(n+1))

        dxdy L = ( 0,-1)
        dxdy R = ( 0, 1)
        dxdy U = (-1, 0)
        dxdy D = ( 1, 0)


id :: Exp
id = Lam $ Var 1

whenM :: LC Bool -> LC () -> LC ()
whenM b f = b >>= flip when f

putMaybe :: Pretty a => Maybe a -> LC ()
putMaybe (Just a) = do whenM (view clear) (exp .= id)
                       whenM (view exit) $ do
                         pos  .= (0,0)
                         prog .= array ((0,0),(0,0)) [((0,0),'@')]
                       liftIO . P.putStr $ pretty a
                       liftIO $ hFlush stdout
putMaybe Nothing = putErrLn "error: type mismatch"

putErrLn :: String -> LC ()
putErrLn = liftIO . hPutStrLn stderr
