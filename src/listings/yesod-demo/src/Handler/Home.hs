{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.Home where

import           Import

import           Data.HashMap.Strict   as HM
import           qualified Text.Blaze.Html5      as Html
import           Yesod.Form.Bootstrap3

import           Article

allArticles :: Handler [Article]
allArticles =
  HM.elems . appArticles <$> getYesod

getArticle :: ArticleId -> Handler Article
getArticle id' = do
  articles <- appArticles <$> getYesod
  maybe notFound pure (HM.lookup id' articles)

getArticleComments :: ArticleId -> Handler [Comment]
getArticleComments id' = do
  postsVar <- appArticleComments <$> getYesod
  HM.lookupDefault [] id' <$> readMVar postsVar

addArticleComment :: ArticleId -> Comment -> Handler ()
addArticleComment id' comment = do
  postsVar <- appArticleComments <$> getYesod
  modifyMVar_ postsVar (return . HM.insertWith (<>) id' [comment])

-- start snippet get-home-handler
getHomeR :: Handler Html
getHomeR = do
  articles <- allArticles
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

-- start snippet get-article-handler
getArticleR :: ArticleId -> Handler Html
getArticleR id' = do
  article <- getArticle id'
  comments <- getArticleComments id'
  defaultLayout $ do
    setTitle (Html.text (articleTitle article))
    $(widgetFile "article")
-- end snippet get-article-handler

-- start snippet get-article-with-form-handler
getArticleWithFormR :: ArticleId -> Handler Html
getArticleWithFormR id' = do
  article <- getArticle id'
  comments <- getArticleComments id'

  (commentFormWidget, commentFormEnc) <-
    generateFormPost (renderForm commentForm)

  defaultLayout $ do
    setTitle (Html.text (articleTitle article))
    $(widgetFile "article-with-form")
-- end snippet get-article-with-form-handler

-- start snippet article-comment-handler
postArticleCommentsR :: ArticleId -> Handler Html
postArticleCommentsR id' = do
  article <- getArticle id'
  comments <- getArticleComments id'

  ((result, commentFormWidget), commentFormEnc) <-
    runFormPost (renderForm commentForm)

  case result of
    FormSuccess comment -> do
      addArticleComment id' comment
      redirect (ArticleWithFormR id')
    _ ->
      defaultLayout $ do
        setTitle (Html.text (articleTitle article))
        $(widgetFile "article-with-form")
-- end snippet article-comment-handler
