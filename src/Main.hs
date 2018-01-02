
module Main where

import Eval
import Types

import System.IO
import System.Console.GetOpt
import System.Environment


main :: IO ()
main = getOpt Permute options <$> getArgs >>= \case
  (args,src:cs,[]) -> evalArgs (readArgs args) src cs
  (_,[],_)      -> die "you need to supply a file name"
  (_,_,err)    -> die $ concat err

  where evalArgs (Opts _ _ True) _   _  = print usage
        evalArgs (Opts True v _) src cs = evalProg v cs src
        evalArgs (Opts _ v _)  fname cs = openFile fname ReadMode >>= hGetContents >>= evalProg v cs

        readArgs = foldr id defaults
        usage = "usage: lam [OPTIONS] [-e expr | file] [INPUTS]"
        die m = ioError $ userError $ m ++ "\n" ++ usageInfo usage options

options = [ Option "e" ["expression"] (NoArg (& expr .~ True)) "use command-line argument as source"
          , Option "v" ["verbose"] (NoArg (& verbose .~ True)) "print debug information"
          , Option "h" ["help"] (NoArg (& help .~ True)) "print this help"
          ]

defaults = Opts False False False
