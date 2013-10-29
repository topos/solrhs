{-# LANGUAGE TemplateHaskell #-}
module Arg (args
           ,usage
           ,host
           ,port
           ,query
           ,requests
           ,concurrency
           ,help) where

import System.Console.GetOpt
import System.Environment (getArgs,getProgName)
import Control.Lens

data Options = Options{_host::String
                      ,_port::Int
                      ,_query::String
                      ,_requests::Int
                      ,_concurrency::Int
                      ,_help::Bool
                      } deriving Show
defaultOptions = Options{_host="localhost"
                        ,_port=8983
                        ,_query="*:*"
                        ,_requests=1
                        ,_concurrency=1
                        ,_help=False}
makeLenses '' Options

options :: [OptDescr (Options -> Options)]
options = [Option ['h'] ["host"]
           (ReqArg (\h opts -> opts{_host=h}) "host")
           "host"
          ,Option ['p'] ["port"]
           (ReqArg (\p opts -> opts{_port=(read p)}) "port")
           "port"
          ,Option ['q'] ["query"]
           (ReqArg (\q opts -> opts{_query=q}) "query")
           "threshold"
          ,Option ['n'] ["requests"]
           (ReqArg (\n opts -> opts{_requests=(read n)}) "requests")
           "requests"
          ,Option ['c'] ["concurrency"]
           (ReqArg (\c opts -> opts{_concurrency=(read c)}) "concurrency")
           "concurrency"
          ,Option ['?'] ["help"]
           (NoArg (\opts -> opts{_help=True}))
           "help"]

cmd :: IO String
cmd = getProgName

args :: IO Options
args = do
  args <- getArgs
  case getOpt RequireOrder options args of
    (opts, [], []) -> return (foldl (flip id) defaultOptions opts)
    (_, _, errs) -> do
      usage' <- usage
      ioError (userError (concat errs ++ usage'))

usage :: IO String
usage = do
  cmd' <- cmd
  let header = "usage: " ++ cmd' ++ " [options...]"
  return $ usageInfo header options
