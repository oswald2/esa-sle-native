{-# LANGUAGE OverloadedStrings 
#-}
module Main where


import           RIO
import qualified Data.Text.IO                  as T

import           SLE.Types
import           SLE.ServiceElement


main :: IO ()
main = do
    se  <- seCreate
    res <- initSLE se "test1" "test2" SleBindRoleResponder "logfile"
    case res of
        Left  err -> T.putStrLn err
        Right ()  -> T.putStrLn "Done."
