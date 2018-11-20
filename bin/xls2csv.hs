#!/usr/bin/env stack
-- stack --resolver lts runhaskell --package getopt-generics

import           System.Environment           (getArgs)
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Resource
import           Data.Conduit
import           Data.Conduit.List            as CL
import           Data.List                    (intercalate)
import           Data.Xls
import           System.Console.GetOpt
import           Data.Maybe                   (fromMaybe)

-- TODO need to escape the separator and the escaping quotes themselves

-- Command line options type
data Options = Options{
      filePath   :: String
    , sheetIndex :: Maybe Int
    }
    deriving Show

-- Reader for sheetIndex
readInt :: String -> Int
readInt = read

-- Command line options descriptors
options :: [OptDescr (Options -> Options)]
options = [
      Option ['F'] ["file"] 
        (ReqArg (\ d opts -> opts {filePath = d}) "FILE")
        "Path to xls file"
    , Option ['i'] ["sheet-index"]
        (OptArg ((\ f opts -> opts {sheetIndex = Just $ readInt f}) . fromMaybe "input") "INDEX")
        "Sheet INDEX starting at 0"
    ]

defaultOptions = Options {
      filePath = ""
    , sheetIndex = Nothing
    }

-- Command line options parser
compilerOpts :: [String] -> IO (Options, [String])
compilerOpts argv =
    case getOpt Permute options argv of
      (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
      (_,_,errs) -> ioError (userError (Prelude.concat errs ++ usageInfo header options))
  where header = "Usage: ic [OPTION...] files..."

-- Positional argument interpreter
hasFileArg :: Options -> [String] -> Options
hasFileArg opts [] = opts
hasFileArg (Options{filePath=f, sheetIndex=i}) [x] = case f of
    "" -> Options{filePath=x, sheetIndex=i}
    str -> case i of
        Just _ -> Options{filePath=str, sheetIndex=i}
        Nothing -> Options{filePath=str, sheetIndex=Just $ readInt x}
hasFileArg (Options{filePath=f, sheetIndex=i}) (x:y:xs) = case f of
    "" -> case i of 
        Just _ -> Options{filePath=x, sheetIndex=i}
        Nothing -> Options{filePath=x, sheetIndex=Just $ readInt y}
    str -> case i of
        Just _ -> Options{filePath=str, sheetIndex=i}
        Nothing -> Options{filePath=str, sheetIndex=Just $ readInt x}

-- Contditional xls conversion
xlsToCSV :: Options -> IO ()
xlsToCSV (Options{filePath=file, sheetIndex=i}) = case i of
    Nothing         -> runResourceT
                     $ runConduit
                     $ decodeXls file -- Dump entire excel sheet
                     .| CL.mapM_ (liftIO . putStrLn . intercalate ",")
    Just sheetIndex -> runResourceT
                     $ runConduit
                     $ decodeXlsSheetNo file sheetIndex -- Only output one sheet
                     .| CL.mapM_ (liftIO . putStrLn . intercalate ",")

main :: IO ()
main = do
  args <- getArgs
  (opts, err) <- compilerOpts args
  opts <- return $ hasFileArg opts err
  xlsToCSV opts
