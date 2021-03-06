
import Control.Monad
import System.Environment (getArgs)

import Types
import Parser
import Math
import CmdLine

readFile' "" = getContents
readFile' x = readFile x

printCoefs (Info _ _ _ a b c _) = putStrLn $ show a ++ " " ++ show b ++ " " ++ show c

printTrend (Info xs ys ts _ _ _ _) = sequence_ $ zipWith3 pr xs ys ts
  where
    pr x y t = putStrLn $ show x ++ "\t" ++ show y ++ "\t" ++ show t

printInfo (Info xs ys _ _ _ _ _) = sequence_ $ zipWith pr xs ys
  where
    pr x y = putStrLn $ show x ++ "\t" ++ show y


main = do
  args <- getArgs
  let (F mode formula, file) = parseCmdLine args
  (xs,ys) <- return . parseColumns =<< readFile' file
  let info = getInfo (length xs) formula xs ys
      printer = case mode of
                  Coefs -> printCoefs
                  TrendColumn -> printTrend
                  SubTrend -> printInfo
                  Predict _ _ -> printTrend
  info' <- case mode of
            SubTrend -> return $ subTrend info
            Predict b n -> predict n b info
            _         -> return info
  printer info'
                
