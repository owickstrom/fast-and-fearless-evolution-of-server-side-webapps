{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Import

import           Post

getHomeR :: Handler Html
getHomeR =
  defaultLayout $ do
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")

getPost :: PostId -> Handler Post
getPost _ = return (Post (PostId "1") "My Cool Post" "This is about stuff.")

getPostComments :: PostId -> Handler [Comment]
getPostComments _ =
  return [Comment "Alice" "This was nice.", Comment "Bob" "Whoa!"]

-- start snippet post-comments-handler
getPostCommentsR :: PostId -> Handler Html
getPostCommentsR pid = do
  post <- getPost pid
  comments <- getPostComments pid
  defaultLayout $ do
    setTitle "Post Comments"
    $(widgetFile "post-comments")
-- end snippet post-comments-handler
