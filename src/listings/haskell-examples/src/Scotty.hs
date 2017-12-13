{-# LANGUAGE OverloadedStrings #-}
module Scotty where

import Control.Monad.IO.Class
import Data.Semigroup
import Data.Text.Lazy
import Web.Scotty
import Lucid

-- start snippet app
app :: ScottyM ()
app = do

  get "/" $
    html "Welcome!"

  get "/greet/:who" $ do
    who <- param "who"
    html ("Hello, " <> who <> "!")
-- end snippet app
  messyHtml
  lucidHandler
  postHandler

-- start snippet lucid-template
homeView :: Text -> Html ()
homeView who =
  html_ [lang_ "en"] $ do
    head_ $ do
      meta_ [charset_ "UTF-8"]
      title_ "My Page"
      link_ [rel_ "stylesheet", href_ bootstrapCss]

    body_ $
      div_ [class_ "jumbotron"] $
        h1_ ("Hello, " <> toHtml who <> "!")
-- end snippet lucid-template
  where
    bootstrapCss = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css"

lucidHandler :: ScottyM ()
lucidHandler =
-- start snippet lucid-handler
  get "/greet-with-lucid/:who" $ do
    who <- param "who"
    html (renderText (homeView who))
-- end snippet lucid-handler

-- start snippet addNewComment
type PostId = Text

addNewComment :: PostId -> Text -> IO ()
--- end snippet addNewComment
addNewComment _ _ = return () -- only for slides

postHandler :: ScottyM ()
postHandler =
  -- start snippet post-handler
  post "/posts/:post-id/comments" $ do
    postId <- param "post-id"
    -- accepts a form or query parameter "comment"
    comment <- param "comment"
    liftIO (addNewComment postId comment)
    redirect ("/posts/" <> postId)
  -- end snippet post-handler

-- start snippet main
main :: IO ()
main = scotty 8080 app
-- end snippet main

messyHtml :: ScottyM ()
messyHtml = do
  let bootstrapCss = "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-beta.2/css/bootstrap.min.css"

  get "/greet-with-template/:who" $ do
    who <- param "who"
    html $
         "<!DOCTYPE html>\
         \<html lang=\"en\">\
         \<head>\
         \  <meta charset=\"UTF-8\">\
         \  <title>My Page</title>\
         \  <link rel=\"stylesheet\"\
         \        href=\"" <> bootstrapCss <> "\">\
         \</head>\
         \<body>\
         \  <div class=\"jumbotron\">\
         \     <h1>Hello, " <> who <> "!</h1>\
         \  </div>\
         \</body>\
         \</html>\
         \"


