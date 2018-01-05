
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
-- "#$%*+,-.0123456789:;<=>?@ABCFGIKLOSTUVWXY[]^_`cfglov~


-- | Evaluate a Lambdoid program
evalProg :: Flags -> [Exp] -> String -> IO ()
evalProg fs as src = do
  out <- (^. exp) <$> execProg initState fs go

  let showNum  = fmap (("Church numeral: "++) . show) . toNum
      showBool = fmap (("Boolean: "++) . show) . toBool
      vals = case mapMaybe ($out) [showNum, showBool] of
               [] -> ""
               vs -> "\t[" ++ intercalate "; " vs ++ "]"

  unless (fs ^. quiet) $
    hPutStrLn stderr $ "\nFinal expression: " ++ show out ++ vals

  where initState = Env (parseSrc src) as id (0,0) R

        -- Loop the program until '@' is reached
        go = getCmd >>= \case '@' -> return ()
                              c   -> evalCmd c >> doForce >> step >> go
        doForce
          | fs ^. force = exp %= simplify
          | otherwise   = return ()


-- | Decide what to do depending on the just read character
evalCmd :: Char -> LC ()
evalCmd c = case c of
  -- Modify the direction pointer
  '<' -> dir .= L
  '>' -> dir .= R
  '^' -> dir .= U
  'v' -> dir .= D
  '?' -> liftIO (toEnum <$> randomRIO (0, 3)) >>= (dir .=)
  'f' -> exp %= simplify
  '$' -> -- Use one command-line argument
    use args >>= \case
      (a:as) -> args .= as >> appExp a
      []     -> appExp (number 0)
  '~' -> -- Ask user for input & apply it
    parseInput <$> liftIO getLine >>= \case
      Left _  -> return ()
      Right e -> appExp e
  ':' -> -- Output the current lambda term
    use exp >>= putPretty
  ';' -> -- Output value as Bool
    toBool <$> use exp >>= putPretty
  ',' -> -- Output value as ASCII char
    toChar <$> use exp >>= putPretty
  '.' -> -- Output value as Number
    toNum <$> use exp >>= putPretty
  '#' -> -- Jump instruction
    step
  'c' -> -- Replace current expression with id
    exp .= id
  'l' -> -- Print newline
    putPretty "\n"
  '"' -> -- Accumulate number & apply it
    step >> accumNumber >>= appExp
  _ | c `elem` "0123456789" -> -- Apply a number literal
        appExp . number $ toNumber c
    | c `elem` map fst opTable -> -- Apply an expression from opTable
        appExp $ fromChar c
    | otherwise -> -- No-op
        return ()

  where appExp :: Exp -> LC ()
        appExp e = exp %= (.$ e)

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
step = do checkModifying
          e  <- get
          pos %= next (snd . bounds $ e ^. prog) (dxdy $ e ^. dir)

  where next (m,n) (dx,dy) (x,y) = (mod(x+dx)(m+1), mod(y+dy)(n+1))

        dxdy L = ( 0,-1)
        dxdy R = ( 0, 1)
        dxdy U = (-1, 0)
        dxdy D = ( 1, 0)


-- | Check if a source code modifying function is evaluated and possibly fire it
checkModifying :: LC ()
checkModifying = use exp >>= \case
  Tri a b c -> case liftM3 (,,) (toNum a) (toNum b) (toChar c) of
                 Just (x,y,c) -> do
                   (m,n) <- snd . bounds <$> use prog
                   let p = (mod(fromIntegral x)(m+1),mod(fromIntegral y)(n+1))
                   prog %= (// [(p,c)])
                   whenM (view verbose) $
                     putErrLn $ "Modify source " ++ show p ++ " -> " ++ show c
                 Nothing -> return ()
  _ -> return ()

  where


id :: Exp
id = Lam $ Var 1

whenM :: LC Bool -> LC () -> LC ()
whenM b f = b >>= flip when f

putPretty :: Pretty a => a -> LC ()
putPretty a = do whenM (view clear) (exp .= id)
                 whenM (view exit) $ do
                   pos  .= (0,0)
                   prog .= array ((0,0),(0,0)) [((0,0),'@')]
                 case pretty a of
                   "" -> whenM (not <$> view quiet) $
                           putErrLn "error: type mismatch"
                   x  -> liftIO $ P.putStr x

putErrLn :: String -> LC ()
putErrLn = liftIO . hPutStrLn stderr

--prog2str :: Prog -> [String]
--prog2str = map (map snd)
--         . map (sortOn (snd . fst))
--         . groupBy (\x y-> fst (fst x) == fst (fst y))
--         . sortOn (fst . fst)
--         . assocs
