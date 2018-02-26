{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Codec.Binary.UTF8.String as UTF8
import           Data.Aeson
import           Data.Maybe
import           Data.Text
import           GHC.Generics
import           Lib
import qualified Network.HTTP.Simple      as HTTP
import           System.Environment       (getArgs)

data GithubRes = GithubRes
                 { number    :: Int
                 , createdAt :: String
                 , title     :: Text
                 } deriving Show

instance FromJSON GithubRes where
    parseJSON (Object v) = GithubRes <$> v .: "number" <*> v .: "created_at" <*>  v .: "title"

main :: IO ()
main = do
    args <- getArgs
    case args of
        "help" : _              -> runHelp
        _ : "help" : _          -> runHelp
        [user, project, _count] -> run user project
        _                       -> runHelp

runHelp :: IO ()
runHelp = putStrLn "usage: issues <user> <project> [ count | #{defaultCount} ]"

run :: String -> String -> IO ()
run user project = do
    req <- HTTP.parseRequest $ "https://api.github.com/repos/" ++ user ++ "/" ++ project ++ "/issues"
    let request = HTTP.setRequestHeader "User-Agent" ["Haskell issues"] req
    res <- HTTP.httpLbs request
    let json = decode (HTTP.getResponseBody res) :: Maybe [GithubRes]
    case json of
        Nothing        -> putStrLn "parsing failed"
        Just githubRes -> print githubRes
