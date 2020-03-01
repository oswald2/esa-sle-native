{-# LANGUAGE
  OverloadedStrings
#-}
module Main where


import           RIO
import qualified RIO.Text                      as T
import qualified Data.Text.IO                  as T
import           Data.SLE.TMLProtocol
import           Data.SLE.TMLConfig
import           State.AppState

main :: IO ()
main = do
  let addr = ConnectAddr { host = "localhost", port = 55529 }

  defLogOptions <- logOptionsHandle stdout True
  let logOptions = setLogMinLevel LevelDebug defLogOptions
  withLogFunc logOptions $ \logFunc -> do
    state <- initialState defaultConfig logFunc (T.putStrLn . T.pack . show)

    runRIO state $ do
      connectSLE addr
