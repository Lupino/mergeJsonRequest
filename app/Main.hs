module Main
  ( main
  ) where

import           Data.Aeson                 (encode)
import qualified Data.ByteString.Char8      as B (getContents)
import qualified Data.ByteString.Lazy.Char8 as LB (putStrLn)
import           Data.GraphQL               (graphql)
import           Data.Text.Encoding         (decodeUtf8)
import           GraphQL                    (schema)

import           Data.Maybe                 (fromMaybe, listToMaybe)
import           Data.String.Utils          (startswith)
import           Network.HTTP.Client        (Manager, defaultManagerSettings,
                                             managerConnCount,
                                             managerResponseTimeout, newManager,
                                             responseTimeoutMicro)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           System.Environment         (getArgs)

main :: IO ()
main = do
  root <- getRoot
  mgr <- initMgr root
  ql <- decodeUtf8 <$> B.getContents
  rsp <- graphql (schema root mgr)  ql

  LB.putStrLn $ encode rsp


getRoot :: IO String
getRoot = fromMaybe root . listToMaybe <$> getArgs
  where root = "https://gw.huabot.com"

initMgr :: String -> IO Manager
initMgr root = newManager settings
  { managerConnCount = 100
  , managerResponseTimeout = responseTimeoutMicro $ 300 * 1000000
  }

  where settings = if startswith "https" root then tlsManagerSettings
                                              else defaultManagerSettings
