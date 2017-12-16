{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Article where

import           Import.NoFoundation

import qualified Data.Text           as T

newtype ArticleId =
  ArticleId Text
  deriving (Show, Read, Eq, Ord, Hashable)

instance PathPiece ArticleId where
  toPathPiece (ArticleId t) = t
  fromPathPiece t
    | not (T.null t) = Just (ArticleId t)
    | otherwise = Nothing

data Article = Article
  { articleId :: ArticleId
  , articleTitle :: Text
  , articleContent :: Text
  }

data Comment = Comment
  { commentAuthor :: Text
  , commentContents :: Text
  }
