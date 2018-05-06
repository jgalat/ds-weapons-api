{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Data.ByteString.Lazy as BSL
import           Data.Maybe
import           System.Exit
import           Data.Aeson
import           Web.StaticAPI

import           API
import           Weapon

jsonFile :: FilePath
jsonFile = "webscrap/weapons.json"

readJSON :: IO (Maybe Weapons)
readJSON = do
  json <- BSL.readFile jsonFile
  return (decode json)

failedParsing :: IO ()
failedParsing = do
  putStrLn "Failed to parse json file."
  exitFailure

runStaticAPI :: Weapons -> IO ()
runStaticAPI weapons = do
  putStrLn "Building API."
  staticAPI (weaponsStaticAPI weapons) defaultOpts
  exitSuccess

main :: IO ()
main = readJSON >>= maybe failedParsing runStaticAPI
