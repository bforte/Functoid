
module Main where

import CombinatoryLogic
import Eval
import Parser
import Types

import System.IO
import System.Console.GetOpt
import System.Environment


main :: IO ()
main = getOpt Permute options <$> getArgs >>= \case
  (args,src:cs,[]) -> do hSetBuffering stdout NoBuffering
                         hSetBuffering stderr NoBuffering
                         case mapM parseInput cs of
                           Left err -> die err
                           Right as -> evalArgs (readArgs args) src as
  (_,[],_)  -> die "you need to supply a file name or expression"
  (_,_,err) -> die $ concat err

  where evalArgs (Opts _ _ True) exp _  = either putStrLn print (parseExp exp >>= toCL)
        evalArgs (Opts True f _) src as = evalProg f as src
        evalArgs (Opts _ f _)  fname as = openFile fname ReadMode >>=
                                            hGetContents >>= evalProg f as

        readArgs = foldr id defaults
        usage = "usage: functoid [OPTIONS] [-e expr | -t expr | file] [INPUTS]"
        die m = ioError $ userError $ m ++ "\n" ++ usageInfo usage options


-- | Command-line options
options :: [OptDescr (Opts -> Opts)]
options = [ Option "e" ["expression"] (NoArg (& expr .~ True)) "use command-line argument as source"
          , Option "f" ["force"] (NoArg (& flags . force .~ True)) "always force evaluation"
          , Option "n" ["no-clear"] (NoArg (& flags . clear .~ False)) "don't clear the current lambda term when printing"
          , Option "v" ["verbose"] (NoArg (& flags . verbose .~ True)) "print steps taken"
          , Option "q" ["quiet"] (NoArg (& flags . quiet .~ True)) "don't print the final lambda term"
          , Option "t" ["transform"] (NoArg (& transform .~ True)) "transform lambda expression to SKIBCW"
          , Option "x" ["exit"] (NoArg (& flags . exit .~ True)) "automatically exit on first print statement"
          ]
