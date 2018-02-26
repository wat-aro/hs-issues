{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Maybe
import           Data.Text           as T
import qualified Data.Text.IO        as IOT
import           GHC.Generics
import           Lib
import qualified Network.HTTP.Simple as HTTP
import           System.Environment  (getArgs)

data GithubRes = GithubRes
                 { number    :: Int
                 , createdAt :: Text
                 , title     :: Text
                 }

githubResToText :: GithubRes -> Text
githubResToText (GithubRes number createdAt title) = "GithubRes { number: " `T.append` (pack . show) number `T.append` ", createdAt: " `T.append` createdAt `T.append` ", title: " `T.append` title `T.append` " }"

githubResListToText :: [GithubRes] -> Text
githubResListToText [] = ""
githubResListToText (x:xs) = do
    "[ " `T.append` f (x:xs)
  where
    f [] = " ]"
    f (x:xs) = do
         githubResToText x `T.append` f xs

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
        Just githubRes -> IOT.putStrLn $ githubResListToText githubRes
