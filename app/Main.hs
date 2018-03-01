{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import qualified Data.List           as List
import           Data.Text           as T
import qualified Data.Text.IO        as IOT
import           Issues.GitHubRes
import qualified Network.HTTP.Simple as HTTP
import           System.Environment  (getArgs)

format :: [(Text, Text, Text)] -> [Text]
format xs =
    header : separator : List.map (\x -> appendText " | " ' ' x widths) xs
  where
    widths :: (Int, Int, Int)
    widths = sizeTpl $ headerTpl : xs

    headerTpl :: (Text, Text, Text)
    headerTpl = ("number", "createdAt", "title")

    header :: Text
    header = appendText " | " ' ' headerTpl widths

    separator :: Text
    separator = appendText "-+-" '-' ("-", "-", "-") widths

sizeTpl :: [(Text, Text, Text)] -> (Int, Int, Int)
sizeTpl = List.foldr (\(t1, t2, t3) (n, m, l) -> (max n $ T.length t1, max m $ T.length t2, max l $ T.length t3))
                     (0, 0, 0)

appendText :: Text -> Char -> (Text, Text, Text) -> (Int, Int, Int) -> Text
appendText separator c (t1, t2, t3) (n, m, l) = T.intercalate separator [T.center n c t1, T.justifyLeft m c t2, T.justifyLeft l c t3]

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
        Just githubRes -> mapM_ IOT.putStrLn $ format . List.map toTextList . List.take count . List.sortBy (flip compare) $ githubRes
