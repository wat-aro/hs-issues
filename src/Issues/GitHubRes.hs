{-# LANGUAGE OverloadedStrings #-}
-- | API Github Response

module Issues.GitHubRes (GithubRes(..), toTextList )where

import           Data.Aeson
import           Data.Function   (on)
import qualified Data.List       as List
import           Data.Monoid
import           Data.Text       as T
import           Data.Time.Clock

data GithubRes = GithubRes
                 { number    :: Int
                 , createdAt :: UTCTime
                 , title     :: Text
                 } deriving Eq

instance FromJSON GithubRes where
    parseJSON (Object v) = GithubRes <$> v .: "number" <*> v .: "created_at" <*>  v .: "title"

instance Ord GithubRes where
    x `compare` y = List.foldr (\f acc -> f x y <> acc) EQ [compare `on` createdAt, compare `on` number, compare `on` title]

toTextList :: GithubRes -> (Text, Text, Text)
toTextList g = (pack . show . number $ g, pack . show . createdAt $ g, title g)
