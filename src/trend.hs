
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

printCoefs settings result = do
  putStrLn $ show (coefA result) ++ " " ++ show (coefB result) ++ " " ++ show (coefC result)
  when (osStdDev settings) $
    print $ stdDev result

printTrend settings result = do
    sequence_ $ zipWith3 pr (xvals result) (yvals result) (trend result)
    when (osStdDev settings) $
      print $ stdDev result
  where
    pr x y t = putStrLn $ show x ++ "\t" ++ show y ++ "\t" ++ show t

printSub settings result = do
    zipWithM_ pr (xvals result) (yvals result)
    when (osStdDev settings) $
      print $ stdDev result
  where
    pr x y = putStrLn $ show x ++ "\t" ++ show y


main = do
  cmdline <- execParser parserInfo
  let parser = if clByCategory cmdline
                 then parseColumnsWithCategories
                 else parseColumns
  input <- parser (clParser cmdline) <$> readFile' (clInput cmdline)
  {-forM_ (M.elems input) $ \ds -> do
    let n = length (dsX ds)
        a = systemLinear n ds
    print a-}
  {-forM_ (M.elems input) $ \ds -> do
    let xs = map toDouble $ dsX ds
    print xs
    print $ zipWith (-) (tail xs) xs-}

  let info = calculateMany (clFormula cmdline) input
      printer = case clMode cmdline of
                  Coefs -> printCoefs
                  TrendColumn -> printTrend
                  SubTrend -> printSub
                  Predict _ _ -> printTrend
      printerMany res =
        case M.size res of
          1 -> let key = head (M.keys res)
                   info = res M.! key
               in  printer (clOutput cmdline) info
          _ -> forM_ (M.assocs res) $ \(category, info) -> do
                 putStr $ category ++ "\t"
                 printer (clOutput cmdline) info
  info' <- case clMode cmdline of
            SubTrend -> return $ subTrendMany info
            Predict b n -> predictMany n b info
            _         -> return info
  printerMany info'
                
