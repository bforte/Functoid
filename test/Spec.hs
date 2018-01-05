
-- Tests for LambdaCalc evaluations:
--  + logic:
--    * not
--    * and
--    * or
--    * xor
--  + arithmetic tests for small numbers:
--    * add
--    * sub
--    * mul
--    * pow
--    * succ
--    * pred
--  + comparisons for small numbers:
--    * eq
--    * leq
--    * le
--    * geq
--    * ge
--  * SKI -> BCKW
--  * BCKW -> SKI

import LambdaCalc
import System.Exit

main :: IO ()
main = do putStrLn "\n------------ Running tests ------------"
          e <- if null tests then do putStrLn "All tests OK"
                                     return ExitSuccess
                             else do putStrLn "The following failed: "
                                     mapM_ (putStrLn . (" - "++)) tests
                                     return $ ExitFailure 1
          putStrLn "---------------------------------------"
          exitWith e

  where -- list of names with failed tests
        tests = logic ++ arithmetic ++ ski_bckw

logic :: [String]
logic = ["not true"  | false /= toExp 'n' .$ true]
             ++ ["not false" | true /= toExp 'n' .$ false]
             ++ concat [testBin n op (toExp c) | (n,op,c) <- binops]

  where testBin n op lop = [n ++ show (a,b) | ((a,x),(b,y)) <- (,) <$> bs <*> bs
                                            , Just (a `op` b) /= toBool (lop .$ x .$ y)]
          where bs = [(True,true),(False,false)]

        binops = [("and",(&&),'A'),("or",(||),'V'),("xor",(/=),'X')]


arithmetic :: [String]
arithmetic = concat [ (s++).show <$> testBinaryNum (binOp o c) l h | (s,o,c,l,h) <- aTests ]
          ++ concat [ (s++).show <$> testBinaryNum (cmpOp o c) l h | (s,o,c,l,h) <- cmpTests ]
          ++ (("sub "++).show <$> testBinaryNum subPred 0 20)
          ++ succTest ++ predTest
  where aTests = [ ("add ",(+),'+',0,20)
                 , ("pow ",(^),'`',1,5)
                 , ("mul ",(*),'*',0,20)
                 ]
        binOp op chr a b = op' .$ number a .$ number b == number (a `op` b)
          where op' = toExp chr

        testBinaryNum pred lo hi = [(a,b) | a <- [lo..hi], b <- [lo..hi], not $ pred a b]

        cmpTests = [ ("eq",(==),'=',0,20)
                   , ("leq ",(<=),'L',0,20)
                   , ("le ",(<),'l',0,20)
                   , ("geq ",(>=),'G',0,20)
                   , ("ge ",(>),'g',0,20)
                   ]
        cmpOp op chr a b = (op' .$ number a .$ number b == true) == (a `op` b)
          where op' = toExp chr

        subPred a b = if a - b < 0 then number 0 == toExp '-' .$ number a .$ number b
                                   else number (a-b) == toExp '-' .$ number a .$ number b

        succTest = ["succ " ++ show n | n <- [0..20], toExp ']' .$ number n /= number (n+1)]
        predTest = ["pred " ++ show n | n <- [1..20], toExp '[' .$ number n /= number (n-1)]


ski_bckw :: [String]
ski_bckw = [ t | (failed, t) <-
              [ (b /= (s .$ (k .$ s)) .$ k, "b")
              , (c /= s .$ (s .$ (k .$ (s .$ (k .$ s) .$ k)) .$ s) .$ (k .$ k), "c")
              , (w /= s .$ s .$ (s .$ k), "w")
              , (i /= w .$ k, "i")
              , (s /= b .$ (b .$ (b .$ w) .$ c) .$ (b .$ b), "s0")
              , (s /= b .$ (b .$ w) .$ (b .$ b .$ c), "s1")
              ]
           , failed]
  where b = toExp 'B'
        c = toExp 'C'
        w = toExp 'W'
        i = toExp 'I'
        s = toExp 'S'
        k = Lam (Lam $ Var 2)

true   = toExp 'T'
false  = toExp 'F'

toExp :: Char -> Exp
toExp c = head [e | (c',e) <- opTable, c' == c]
