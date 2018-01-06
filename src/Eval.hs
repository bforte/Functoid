
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
-- "#$%*+,-.0123456789:;<=>?@ABCEFGIKLORSTUVWXY[]^_`fgilnorv|~


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
    use exp >>= putPretty
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
  '(' -> -- Collect functions to compose and apply it to expression
        step >> accumFuncs c >>= appExp
  ')' -> -- Collect functions to compose and apply expression to it
        step >> accumFuncs c >>= (exp%=).(.$)
  _ | c `elem` digits -> -- Apply a number literal
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
                       c | c `elem` "<>^v"  -> evalCmd c >> step >> go
                         | c == '@'         -> exitProgram >> return []
                         | otherwise        -> step >> (toNumber c :) <$> go

        toNumber :: Char -> Integer
        toNumber c
          | c `elem` digits = fromIntegral $ fromEnum c - 48
          | otherwise = fromIntegral $ fromEnum c

        digits :: [Char]
        digits = "0123456789"

        accumFuncs :: Char -> FC Exp
        accumFuncs p = go id
          where go f = getCmd >>= \case
                         '"' -> do a <- step >> accumNumber
                                   go (f .$ a)
                         c | c `elem` "<>^v"   -> evalCmd c >> step >> go f
                           | c `elem` digits   -> step >> go (f .$ number (toNumber c))
                           | c == '@'          -> exitProgram >> return f
                           | c == close p      -> return f
                           | c == '('          -> do g <- step >> accumFuncs c
                                                     step >> go (f .$ g)
                           | c == ')'          -> do g <- step >> accumFuncs c
                                                     step >> go (g .$ f)
                           | c `elem` builtins -> step >> go (f .$ toExp c)
                           | otherwise         -> step >> go  f

                close c | c == '('  = ')'
                        | otherwise = '('


-- | Get the current command that gets executed
getCmd :: FC Char
getCmd = do
  env <- get
  whenM (view verbose) $
    putErrLn $ show (env ^. pos) ++ " [" ++ show (env ^. dir) ++ "]"
  return $ (env ^. prog) ! (env ^. pos)


-- | Step the program; move command pointer in the current direction
step :: FC ()
step = do checkAction
          e  <- get
          pos %= next (snd . bounds $ e ^. prog) (dxdy $ e ^. dir)

  where next (m,n) (dx,dy) (x,y) = (mod(x+dx)(m+1), mod(y+dy)(n+1))

        dxdy L = ( 0,-1)
        dxdy R = ( 0, 1)
        dxdy U = (-1, 0)
        dxdy D = ( 1, 0)


-- | Check if an action is evaluated and possibly fire it
checkAction :: FC ()
checkAction = use exp >>= \case
  Tri a b c -> case liftM3 (,,) (toNum a) (toNum b) (toChar c) of
                 Just (x,y,c) -> do
                   (m,n) <- snd . bounds <$> use prog
                   let p = (mod(fromIntegral x)(m+1),mod(fromIntegral y)(n+1))
                   prog %= (// [(p,c)])
                   whenM (view verbose) $
                     putErrLn $ "Modify source " ++ show p ++ " -> " ++ show c
                 Nothing -> return ()
  Lit Exit -> exitProgram
  Lit Reset -> exp .= id
  _ -> return ()


exitProgram :: FC ()
exitProgram = pos .= (0,0) >> prog .= array ((0,0),(0,0)) [((0,0),'@')]

id :: Exp
id = Lam $ Var 1

whenM :: FC Bool -> FC () -> FC ()
whenM b f = b >>= flip when f

putPretty :: Pretty a => a -> FC ()
putPretty a = do whenM (view clear) (exp .= id)
                 whenM (view exit) exitProgram
                 case pretty a of
                   "" -> whenM (not <$> view quiet) $
                           putErrLn "error: type mismatch"
                   x  -> liftIO $ P.putStr x

putErrLn :: String -> FC ()
putErrLn = liftIO . hPutStrLn stderr

--dbg :: Show a => a -> FC a
--dbg a = putErrLn (show a) >> return a

--prog2str :: Prog -> [String]
--prog2str = map (map snd)
--         . map (sortOn (snd . fst))
--         . groupBy (\x y-> fst (fst x) == fst (fst y))
--         . sortOn (fst . fst)
--         . assocs
