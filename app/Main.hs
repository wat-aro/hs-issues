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
                 } deriving Eq

instance FromJSON GithubRes where
    parseJSON (Object v) = GithubRes <$> v .: "number" <*> v .: "created_at" <*>  v .: "title"

instance Ord GithubRes where
    x `compare` y = List.foldr (\f acc -> (f x y) <> acc) EQ [compare `on` createdAt, compare `on` number, compare `on` title]

infixl 0 |>
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)

lengthList :: [GithubRes] -> [Int]
lengthList = List.map T.length headersList
             |> List.foldr (\x [a, b, c] -> [ max (T.length . pack . show . number $ x) a
                                            , max (T.length . pack . show . createdAt $ x) b
                                            , max (T.length . title $ x) c
                                            ])

formatText :: [GithubRes] -> Text
formatText githubResList =
    T.intercalate "\n" (headers : separator : contents)
  where
    width = lengthList githubResList
    headers = T.intercalate " | " $ List.map (\(header, i) -> T.justifyLeft i ' ' header) $ List.zip headersList width
    separator = T.intercalate "-+-" $ List.map (flip T.replicate "-") width
    contents :: [Text]
    contents = githubResList
               |> List.map (\x -> width
                                  |> List.zip (toText x)
                                  |> List.map (\(text, i) -> T.justifyLeft i ' ' text))
               |> List.map (T.intercalate " | ")

toText :: GithubRes -> [Text]
toText gr = [pack . show . number $ gr, pack . show . createdAt $ gr, title gr]


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

headersList :: [Text]
headersList = ["number", "created_at", "title"]

run :: String -> String -> Int -> IO ()
run user project count = do
    req <- HTTP.parseRequest $ "https://api.github.com/repos/" ++ user ++ "/" ++ project ++ "/issues"
    let request = HTTP.setRequestHeader "User-Agent" ["Haskell issues"] req
    res <- HTTP.httpLbs request
    let json = decode (HTTP.getResponseBody res) :: Maybe [GithubRes]
    case json of
        Nothing        -> putStrLn "parsing failed"
        Just githubRes -> IOT.putStrLn . formatText . List.take count . List.reverse . List.sort $ githubRes
