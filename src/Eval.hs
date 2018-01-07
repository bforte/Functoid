
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


-- Characters [\33..\126] by usage:
--   "#$%& ()*+,-. 0123456789:;<=>?@ABC EFG I KL  O  RSTUVWXYZ[ ]^_` b   fghi  l n pqr t v     | ~
--  !     '       /                    D   H J  MN PQ          \    a cde    jk m o   s u wxyz{ }


-- | Evaluate a Functoid program
evalProg :: Flags -> [Exp] -> String -> IO ()
evalProg fs as src = do
  out <- simplify . (^. exp) <$> execProg initState fs go

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
          | fs ^. force = exp %!= simplify
          | otherwise   = return ()


-- | Decide what to do depending on the just read character
evalCmd :: Char -> FC ()
evalCmd c = case c of
  -- Modify the direction pointer
  '<' -> dir .= L
  '>' -> dir .= R
  '^' -> dir .= U
  'v' -> dir .= D
  '?' -> liftIO (toEnum <$> randomRIO (0, 3)) >>= (dir .=)
  '_' -> -- Set direction to right iff expression is 0; left otherwise
    toBool <$> use exp >>= \case
      Just False -> dir .= R
      _          -> dir .= L
  '|' -> -- Set direction to down iff expression is 0; up otherwise
    toBool <$> use exp >>= \case
      Just False -> dir .= D
      _          -> dir .= U
  'f' -> exp %!= simplify
  '$' -> -- Use one command-line argument
    use args >>= \case
      (a:as) -> args .= as >> appExp a
      []     -> appExp (number 0)
  '~' -> -- Ask user for input & apply it
    parseInput <$> liftIO getLine >>= \case
      Left _  -> return ()
      Right e -> appExp e
  ':' -> -- Output the current lambda term
    simplify <$> use exp >>= putPretty
  ';' -> -- Output value as Bool
    toBool <$> use exp >>= putPretty
  ',' -> -- Output value as ASCII char
    toChar <$> use exp >>= putPretty
  '.' -> -- Output value as Number
    toNum <$> use exp >>= putPretty
  '#' -> -- Jump instruction
    step
  'r' -> -- Replace current expression with id
    exp .= id
  'p' -> -- Print newline
    putPretty "\n"
  '"' -> -- Accumulate number & apply it
    step >> accumNumber >>= appExp
  _ | c `elem` "()" -> -- Evaluate nested expression
        nestedExpr c
    | c `elem` digits -> -- Apply a number literal
        appExp . number $ toNumber c
    | c `elem` builtins -> -- Apply an expression from opTable
        appExp $ toExp c
    | otherwise -> -- No-op
        return ()

  where appExp :: Exp -> FC ()
        appExp e = exp %= (.$ e)

        accumNumber :: FC Exp
        accumNumber = number . foldl ((+).(10*)) 0 <$> go
          where go = getCmd >>= \case
                       '"'                  -> return []
                       c | c == '@'         -> exitProgram >> return []
                         | c `elem` "<>^v?" -> evalCmd c >> step >> go
                         | otherwise        -> step >> (toNumber c :) <$> go

        toNumber :: Char -> Integer
        toNumber c
          | c `elem` digits = fromIntegral $ fromEnum c - 48
          | otherwise = fromIntegral $ fromEnum c

        digits :: [Char]
        digits = "0123456789"

        nestedExpr :: Char -> FC ()
        nestedExpr p = do
          before <- use exp
          exp .= id
          go
          after <- use exp

          if p == ')' then exp .= after .$ before
                      else exp .= before .$ after

          where go = step' False >> getCmd >>= \case
                       '@'            -> exitProgram
                       c | c == close -> return ()
                         | otherwise  -> evalCmd c >> go

                close | p == '('  = ')'
                      | otherwise = '('


-- | Get the current command that gets executed
getCmd :: FC Char
getCmd = do
  env <- get
  let p = env ^. pos
      c = (env ^. prog) ! p
  whenM (view verbose) $
    putErrLn $ show (yx2xy p) ++ " " ++ show c ++ " [" ++ show (env ^. dir) ++ "]"
  return c


-- | Step the program; move command pointer in the current direction
step :: FC ()
step = step' True

-- | Step the program; but only check for actions if flag set
step' :: Bool -> FC ()
step' c = do if c then checkAction
                  else return ()
             e  <- get
             pos %= next (snd . bounds $ e ^. prog) (dydx $ e ^. dir)

  where next (m,n) (dy,dx) (y,x) = (mod(y+dy)(m+1), mod(x+dx)(n+1))

        dydx L = ( 0,-1)
        dydx R = ( 0, 1)
        dydx U = (-1, 0)
        dydx D = ( 1, 0)


-- | Check if an action is evaluated and possibly fire it
checkAction :: FC ()
checkAction = use exp >>= \case
  Tri a b c -> case liftM3 (,,) (toNum a) (toNum b) (toChar c) of
                 Just (x,y,c) -> do
                   (m,n) <- snd . bounds <$> use prog
                   let p = (mod(fromIntegral y)(m+1),mod(fromIntegral x)(n+1))
                   prog %= (// [(p,c)])
                   exp .= id
                   whenM (view verbose) $
                     putErrLn $ "Modify source " ++ show (yx2xy p) ++ " -> " ++ show c
                 Nothing -> return ()
  Lit Exit -> exitProgram
  Lit Reset -> exp .= id
  _ -> return ()


exitProgram :: FC ()
exitProgram = pos .= (0,0) >> prog .= array ((0,0),(0,0)) [((0,0),'@')]


putPretty :: Pretty a => a -> FC ()
putPretty a = do whenM (view clear) (exp .= id)
                 whenM (view exit) exitProgram
                 case pretty a of
                   "" -> whenM (not <$> view quiet) $
                           putErrLn "error: type mismatch"
                   x  -> liftIO $ P.putStr x


id :: Exp
id = Lam $ Var 1

whenM :: FC Bool -> FC () -> FC ()
whenM b f = b >>= flip when f

putErrLn :: String -> FC ()
putErrLn = liftIO . hPutStrLn stderr

yx2xy :: (a,b) -> (b,a)
yx2xy (a,b) = (b,a)


-- | Debugging related stuff

--dbg :: Show a => a -> FC a
--dbg a = putErrLn (show a) >> return a

--dbgEval :: Bool -> String -> IO ()
--dbgEval v = evalProg (defaults ^. flags & verbose .~ v) []

--prog2str :: Prog -> [String]
--prog2str = map (map snd)
--         . map (sortOn (snd . fst))
--         . groupBy (\x y-> fst (fst x) == fst (fst y))
--         . sortOn (fst . fst)
--         . assocs
