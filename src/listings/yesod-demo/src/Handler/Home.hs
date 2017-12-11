{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Import

import           Post
import Text.Blaze.Html5 as Html

allPosts :: Handler [Post]
allPosts =
  return
    [ Post
        (PostId "fast-and-fearless")
        "Fast and Fearless Evolution of Server-Side Web Applications"
        "Lorem ipsum dolor sit amet..."
    , Post
        (PostId "introducing-yesod")
        "Introducing Yesod"
        "Lorem ipsum dolor sit amet..."
    ]

getPost :: PostId -> Handler Post
getPost id' = do
  posts <- allPosts
  maybe notFound pure (find ((==) id' . postId) posts)

getPostComments :: PostId -> Handler [Comment]
getPostComments _ =
  return [Comment "Alice" "This was nice.", Comment "Bob" "Whoa!"]

-- start snippet get-home-handler
getHomeR :: Handler Html
getHomeR = do
  posts <- allPosts
  defaultLayout $ do
    setTitle "My Blog"
    $(widgetFile "homepage")

-- end snippet get-home-handler
-- start snippet get-post-handler
getPostR :: PostId -> Handler Html
getPostR id' = do
  post <- getPost id'
  comments <- getPostComments id'
  defaultLayout $ do
    setTitle (Html.text (postTitle post))
    $(widgetFile "post")
-- end snippet get-post-handler
