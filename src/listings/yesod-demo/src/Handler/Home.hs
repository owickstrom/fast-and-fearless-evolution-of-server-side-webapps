{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Import

import           Data.HashMap.Strict   as HM
import           Text.Blaze.Html5      as Html
import           Yesod.Form.Bootstrap3

import           Post

allPosts :: Handler [Post]
allPosts =
  HM.elems . appPosts <$> getYesod

getPost :: PostId -> Handler Post
getPost id' = do
  posts <- appPosts <$> getYesod
  maybe notFound pure (HM.lookup id' posts)

getPostComments :: PostId -> Handler [Comment]
getPostComments id' = do
  postsVar <- appPostComments <$> getYesod
  HM.lookupDefault [] id' <$> readMVar postsVar

addPostComment :: PostId -> Comment -> Handler ()
addPostComment id' comment = do
  postsVar <- appPostComments <$> getYesod
  modifyMVar_ postsVar (return . HM.insertWith (<>) id' [comment])

-- start snippet get-home-handler
getHomeR :: Handler Html
getHomeR = do
  posts <- allPosts
  defaultLayout $ do
    setTitle "My Blog"
    $(widgetFile "homepage")
-- end snippet get-home-handler

-- start snippet comment-form
commentForm :: AForm Handler Comment
commentForm =
  Comment
  <$> areq textField (named "Name") Nothing
  <*> (unTextarea
       <$> areq textareaField (named "Comment") Nothing)
-- end snippet comment-form
  where
    named :: Text -> FieldSettings App
    named = bfs

renderForm :: FormRender Handler a
renderForm = renderBootstrap3 BootstrapBasicForm

-- start snippet get-post-handler
getPostR :: PostId -> Handler Html
getPostR id' = do
  post <- getPost id'
  comments <- getPostComments id'
  defaultLayout $ do
    setTitle (Html.text (postTitle post))
    $(widgetFile "post")
-- end snippet get-post-handler

-- start snippet get-post-with-form-handler
getPostWithFormR :: PostId -> Handler Html
getPostWithFormR id' = do
  post <- getPost id'
  comments <- getPostComments id'

  (commentFormWidget, commentFormEnc) <-
    generateFormPost (renderForm commentForm)

  defaultLayout $ do
    setTitle (Html.text (postTitle post))
    $(widgetFile "post-with-form")
-- end snippet get-post-with-form-handler

-- start snippet post-comment-handler
postPostCommentsR :: PostId -> Handler Html
postPostCommentsR id' = do
  post <- getPost id'
  comments <- getPostComments id'

  ((result, commentFormWidget), commentFormEnc) <-
    runFormPost (renderForm commentForm)

  case result of
    FormSuccess comment -> do
      addPostComment id' comment
      redirect (PostWithFormR id')
    _ ->
      defaultLayout $ do
        setTitle (Html.text (postTitle post))
        $(widgetFile "post-with-form")
-- end snippet post-comment-handler
