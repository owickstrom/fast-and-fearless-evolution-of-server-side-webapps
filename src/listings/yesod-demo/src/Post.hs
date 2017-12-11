module Post where

import           Import.NoFoundation

import qualified Data.Text           as T

newtype PostId =
  PostId Text
  deriving (Show, Read, Eq, Ord)

instance PathPiece PostId where
  toPathPiece (PostId t) = t
  fromPathPiece t
    | not (T.null t) = Just (PostId t)
    | otherwise = Nothing

data Post = Post
  { postId :: PostId
  , postTitle :: Text
  , postContent :: Text
  }

data Comment = Comment
  { commentAuthor :: Text
  , commentContents :: Text
  }
