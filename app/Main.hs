{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BSL
import           Data.Default.Class (def)
import           Data.Maybe
import           System.Exit
import           System.Environment
import           Data.Aeson
import           Web.StaticAPI

import           API
import           Weapon

readJSON :: [String] -> IO (Maybe Weapons)
readJSON []             = return Nothing
readJSON (jsonFile : _) = decode <$> BSL.readFile jsonFile

failedParsing :: IO ()
failedParsing = do
  putStrLn "Failed to parse json file."
  exitFailure

runStaticAPI :: Weapons -> IO ()
runStaticAPI weapons = do
  putStrLn "Building API."
  staticAPIOpts (weaponsStaticAPI weapons) (def { outputDirectory = "dist" })
  exitSuccess

main :: IO ()
main = getArgs >>= readJSON >>= maybe failedParsing runStaticAPI
