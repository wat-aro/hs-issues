{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Function       (on)
import qualified Data.List           as List
import           Data.Maybe
import           Data.Monoid
import           Data.Text           as T
import qualified Data.Text.IO        as IOT
import           Data.Time.Clock
import           GHC.Generics
import           Lib
import qualified Network.HTTP.Simple as HTTP
import           System.Environment  (getArgs)

data GithubRes = GithubRes
                 { number    :: Int
                 , createdAt :: UTCTime
                 , title     :: Text
                 }

instance FromJSON GithubRes where
    parseJSON (Object v) = GithubRes <$> v .: "number" <*> v .: "created_at" <*>  v .: "title"

instance Eq GithubRes where
    x == y = List.all (\f -> f x y) [(==) `on` number, (==) `on` createdAt, (==) `on` title]

instance Ord GithubRes where
    x `compare` y = List.foldr (\f acc -> (f x y) <> acc) EQ [compare `on` createdAt, compare `on` number, compare `on` title]

githubResToText :: GithubRes -> Text
githubResToText (GithubRes number createdAt title) = List.foldr1 (<>) ["GithubRes { number: ", (pack . show) number, ", createdAt: ", (pack . show) createdAt, ", title: ", title, " }"]

githubResListToText :: [GithubRes] -> Text
githubResListToText [] = ""
githubResListToText (x:xs) = "[ " <> List.foldr (\x acc -> githubResToText x <> "\n, " <> acc) (githubResToText x) xs <> "\n]"

main :: IO ()
main = do
    args <- getArgs
    case args of
        "help" : _             -> runHelp
        [user, project]        -> run user project 10
        [user, project, count] -> run user project (read count :: Int)
        _                      -> runHelp

runHelp :: IO ()
runHelp = putStrLn "usage: issues <user> <project> [ count | #{defaultCount} ]"

run :: String -> String -> Int -> IO ()
run user project count = do
    req <- HTTP.parseRequest $ "https://api.github.com/repos/" ++ user ++ "/" ++ project ++ "/issues"
    let request = HTTP.setRequestHeader "User-Agent" ["Haskell issues"] req
    res <- HTTP.httpLbs request
    let json = decode (HTTP.getResponseBody res) :: Maybe [GithubRes]
    case json of
        Nothing        -> putStrLn "parsing failed"
        Just githubRes -> IOT.putStrLn . githubResListToText . List.take count . List.sort $ githubRes
