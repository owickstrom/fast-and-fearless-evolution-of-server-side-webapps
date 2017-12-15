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

data Post = Post { postTitle :: Text, postContents :: Text }

routingParam :: Monad m => Text -> Webmachine m Text
routingParam t = do
  p <- params
  return (p HM.! t)

textResponse = ResponseBuilder . fromByteString . TE.encodeUtf8

-- Bogus implementation.
getPost :: Text -> Webmachine IO (Maybe Post)
getPost _ =
  return $
    Just Post
    { postTitle = "Airship Webmachines!"
    , postContents = "Lorem ipsum..."
    }

-- Bogus implementation.
postExists :: Text -> Webmachine IO Bool
postExists _ = return True

renderPost :: Post -> Text
renderPost post =
  mconcat
  [ "<h1>"
  , postTitle post
  , "</h1>"
  , "<p>"
  , postContents post
  , "</p>"
  ]

response404 = escapedResponse "Not found!"

postResource :: Resource IO
postResource =
  defaultResource
  {
-- start snippet allowedMethods
    allowedMethods =
      return [HTTP.methodGet, HTTP.methodHead, HTTP.methodPost]
-- end snippet allowedMethods
-- start snippet resourceExists
  , resourceExists =
      routingParam "postId" >>= postExists
-- end snippet resourceExists
-- start snippet contentTypesProvided
  , contentTypesProvided =
    let htmlResponse (Just post) =
          return (textResponse (renderPost post))
        htmlResponse Nothing =
          return response404
    in return [("text/html", routingParam "postId"
                             >>= getPost
                             >>= htmlResponse)]
-- end snippet contentTypesProvided
  }

-- start snippet app-routes
appRoutes :: Resource IO -> RoutingSpec IO ()
appRoutes static = do
  "posts" </> var "postId" #> postResource
  "static" </> star        #> static
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
