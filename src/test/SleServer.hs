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
      port = 55529
  hdl <- newSleHandle port 

  T.putStrLn "Running Server..."

  startServer handler hdl 
  return ()


