{-# LANGUAGE
  OverloadedStrings
#-}
module Main where


import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import           Data.SLE.TMLConfig
import           Data.SLE.Api


main :: IO ()
main = do
  let addr = ConnectAddr { host = "localhost", port = 55529 }

      handler msg = T.putStrLn (T.pack (show msg))

  queue <- newTBQueueIO 5000

  startClient addr handler queue
