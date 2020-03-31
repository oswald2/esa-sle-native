{-# LANGUAGE
  OverloadedStrings
  , NoImplicitPrelude
#-}
module Main where


import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T

import           Data.SLE.Api



main :: IO ()
main = do
  let handler msg = T.putStrLn (T.pack (show msg))

  queue <- newTBQueueIO 5000

  T.putStrLn "Running Server..."

  startServer 55529 handler queue
  return ()


