module Main (main) where

import Control.Concurrent
import Control.Lens ((^.))
import Network.HTTP (getResponseCode,getResponseBody)
import Data.Tuple.Select
import Text.Printf (printf)
import qualified Arg
import Arg (host,port,query,requests,concurrency,help)
import TimeIt (time)
import qualified Network.Search.Solr as Solr
import Network.Search.Solr (SolrInstance(..),solrHost,solrPort)
import qualified Network.Search.Data as SolrData
import Network.Search.Data (SearchQuery(..),SearchParameter(..))

data Async a = Async (MVar a)

async :: IO a -> IO (Async a)
async action = do
    var <- newEmptyMVar
    forkIO (do r <- action; putMVar var r)
    return (Async var)

wait :: Async a -> IO a
wait (Async var) = readMVar var

solr :: String -> Int -> SolrInstance
solr host port = SolrInstance {solrHost=host,solrPort=port}

type QueryString = String
querySolr :: SolrInstance -> QueryString -> IO ()
querySolr solr query = do
  let query' = Keyword query
  (res',time') <- time $ Solr.get solr [query']
  code <- getResponseCode res'
  print $ (show time') ++ ": " ++ (show $ sel1 code)

main = do
  args <- Arg.args
  if not (args^.help) then do
    let solr' = solr (args^.host) (args^.port)
        concurrency' = args^.concurrency
        runners = take concurrency' $ repeat (async $ querySolr solr' (args^.query))
    printf "c=%d\n" concurrency'
    as <- mapM async runners
    rs <- mapM_ wait as
    print rs
    print "done"
  else do
    usage <- Arg.usage
    putStrLn usage
