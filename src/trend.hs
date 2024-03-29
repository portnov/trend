
import Control.Monad
import qualified Data.Map as M
import System.Environment (getArgs)
import Options.Applicative

import Types
import Parser
import Math
import CmdLine

readFile' "-" = getContents
readFile' x = readFile x

printCoefs (RegressionResult _ _ _ a b c _) = putStrLn $ show a ++ " " ++ show b ++ " " ++ show c

printTrend (RegressionResult xs ys ts _ _ _ _) = sequence_ $ zipWith3 pr xs ys ts
  where
    pr x y t = putStrLn $ show x ++ "\t" ++ show y ++ "\t" ++ show t

printInfo (RegressionResult xs ys _ _ _ _ _) = sequence_ $ zipWith pr xs ys
  where
    pr x y = putStrLn $ show x ++ "\t" ++ show y


main = do
  CmdLine mbSep byCategory mode formula file <- execParser parserInfo
  let parser = if byCategory
                 then parseColumnsWithCategories
                 else parseColumns
  input <- parser mbSep <$> readFile' file
  let info = calculateMany formula input
      printer = case mode of
                  Coefs -> printCoefs
                  TrendColumn -> printTrend
                  SubTrend -> printInfo
                  Predict _ _ -> printTrend
      printerMany res =
        case M.size res of
          1 -> let key = head (M.keys res)
                   info = res M.! key
               in  printer info
          _ -> forM_ (M.assocs res) $ \(category, info) -> do
                 putStr $ category ++ "\t"
                 printer info
  info' <- case mode of
            SubTrend -> return $ subTrendMany info
            Predict b n -> predictMany n b info
            _         -> return info
  printerMany info'
                
