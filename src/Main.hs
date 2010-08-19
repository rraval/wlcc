module Main(main) where

import Data.List(foldl')
import System.Console.GetOpt
import qualified System.IO as IO
import System.Environment(getArgs)
import System.Exit(exitFailure, exitSuccess)

import Assembler.Data
import Assembler.Parser
import Assembler.Translator

readBinaryFile :: IO.FilePath -> IO String
readBinaryFile file = do
  handle <- IO.openBinaryFile file IO.ReadMode
  -- FIXME: no call to hClose on handle
  IO.hGetContents handle

writeBinaryFile :: IO.FilePath -> String -> IO ()
writeBinaryFile file contents = do
  handle <- IO.openBinaryFile file IO.WriteMode
  IO.hPutStr handle contents
  IO.hClose handle

data Flags = Flags { input :: IO String
                   , output :: String -> IO ()
                   , version :: IO ()
                   }

options :: [OptDescr (Flags -> Flags)]
options =
  [ Option ['V', '?']   ["version"] (NoArg version)         "show version information"
  , Option ['o']        ["output"]  (ReqArg output "FILE")  "output file"
  ] where version f  = f { version = do
                              IO.hPutStr IO.stderr $ unlines ["wlpp version 0.1"
                                                             ,"Copyright 2010 Ronuk Raval"]
                              exitSuccess
                         }
          output o f = f { output = writeBinaryFile o }

parseOptions :: [String] -> IO (Flags, [String])
parseOptions argv =
  case getOpt (ReturnInOrder input) options argv of
    (o, n, []) -> let apply a f = f a;
                      initial = Flags { input = getContents
                                      , output = putStr
                                      , version = return () }
                  in return $ (foldl' apply initial o, n)
    (_, _, errs) -> ioError (userError (concat errs ++ usageInfo header options))
  where input i f = f { input = readBinaryFile i }
        header =  "Usage: wlpp [OPTION...] INPUT"

main :: IO ()
main = do
  (opts, extra) <- getArgs >>= parseOptions
  version opts
  input opts >>= output opts
