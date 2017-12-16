{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Airship
import           Airship.Resource.Static
import           Data.Binary.Builder      (fromByteString)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Map.Strict          as M
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as TE
import           Lens.Micro
import qualified Network.HTTP.Types       as HTTP
import           Network.Wai.Handler.Warp (defaultSettings, runSettings,
                                           setHost, setPort)

data Article = Article { articleTitle :: Text, articleContents :: Text }

routingParam :: Monad m => Text -> Webmachine m Text
routingParam t = do
  p <- params
  return (p HM.! t)

textResponse = ResponseBuilder . fromByteString . TE.encodeUtf8

-- Bogus implementation.
getArticle :: Text -> Webmachine IO (Maybe Article)
getArticle _ =
  return $
    Just Article
    { articleTitle = "Airship Webmachines!"
    , articleContents = "Lorem ipsum..."
    }

-- Bogus implementation.
articleExists :: Text -> Webmachine IO Bool
articleExists _ = return True

renderArticle :: Article -> Text
renderArticle article =
  mconcat
  [ "<h1>"
  , articleTitle article
  , "</h1>"
  , "<p>"
  , articleContents article
  , "</p>"
  ]

response404 = escapedResponse "Not found!"

articleResource :: Resource IO
articleResource =
  defaultResource
  {
-- start snippet allowedMethods
    allowedMethods =
      return [HTTP.methodGet, HTTP.methodHead, HTTP.methodPost]
-- end snippet allowedMethods
-- start snippet resourceExists
  , resourceExists =
      routingParam "articleId" >>= articleExists
-- end snippet resourceExists
-- start snippet contentTypesProvided
  , contentTypesProvided =
    let htmlResponse (Just article) =
          return (textResponse (renderArticle article))
        htmlResponse Nothing =
          return response404
    in return [("text/html", routingParam "articleId"
                             >>= getArticle
                             >>= htmlResponse)]
-- end snippet contentTypesProvided
  }

-- start snippet app-routes
appRoutes :: Resource IO -> RoutingSpec IO ()
appRoutes static = do
  "articles" </> var "articleId" #> articleResource
  "static"   </> star            #> static
-- end snippet app-routes

main :: IO ()
main = do
  static <- staticResource Cache "assets"
  let port = 3000
      host = "127.0.0.1"
      settings = setPort port (setHost host defaultSettings)
      routes = appRoutes static
      config =
        defaultAirshipConfig & includeTraceHeader .~
        OmitHeader &
        includeQuipHeader .~
        OmitHeader
  putStrLn "Listening on port 3000"
  runSettings settings (resourceToWai config routes errors)
  where
    errors =
      M.singleton
        HTTP.status404
        [("text/html", return response404)]
