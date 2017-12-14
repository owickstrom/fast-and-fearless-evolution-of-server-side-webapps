---
title: Fast and Fearless Evolution of Server-Side Web Applications
author: Oskar Wickström
date: |
  \includegraphics[width=3cm]{../../src/mpowered.png}

theme: Boadilla
classoption: dvipsnames
---

## Outline

* Introduction
* Web Applications
* Writing Web Applications with Haskell
    - Scotty
    - Yesod
    - Airship
* Client-Side Technologies

\notelist{
  \item
}

# Introduction

## Evolving Software

* New features
* Bug fixes
* Refactoring
* External factors
    - New regulation
    - Deprecation of software and services
* Recruiting

\notelist{
  \item We have many reasons to evolve software...
}

## Risk

\begin{columns}[T,onlytextwidth]
  \begin{column}{.4\textwidth}
    \begin{minipage}{\textwidth}
      \begin{itemize}
        \item{Risks
          \begin{itemize}
            \item Delay
            \item Error
            \item Burnout
          \end{itemize}}
      \end{itemize}
    \end{minipage}
  \end{column}
  \begin{column}{.4\textwidth}
    \begin{minipage}{\textwidth}
      \begin{tikzpicture}[black]
        \draw (0,0) node[anchor=north]{Cost}
          -- (3,0) node[anchor=north]{Scope}
          -- (1.5,2.595) node[anchor=south]{Time}
          -- cycle;
      \end{tikzpicture}
    \end{minipage}
  \end{column}
\end{columns}


\notelist{
  \item When evolving software, we risk: ...
  \item These things hurt: Customers, partners, internal relations
  \item This might remind you of the classic project management triangle
  \item I think the fear of evolving software is rooted in past experience
}

## Tooling

* We can reduce fear with better tools
    - Stronger correctness guarantees
    - Robustness
    - Faster feedback
    - Communicates intent
* Functional programming
* Type systems
* Error handling

# Web Applications

## Web Applications

* Many of us work with the web somehow
* Single-page apps (SPAs) are in vogue
    - More like desktop apps
    - Reinventing parts of the browser
    - No Javascript, no application
* Universal webapps (aka "isomorphic")

## Reminder

\centering{\Large{$\text{Newer} \not\Longrightarrow \text{Better}$}}

## Server-Side Web Applications

* Do not dismiss server-side web applications
* Progressive enhancement
* 80/20 rule
* Use client-side code where you need it!
* PJAX

## Static Typing for Server-Side Web

* Compile-time checking
    - Run-time robustness with defined behaviour
    - Use types for correct-by-construction
    - Machine-verified living documentation, communicates intent
* Safely evolve our codebase
    - Good tools reduce fear of change
    - Modify core domain, follow the errors
    - Not split by an API
* Focus tests on our domain
    - No need to write tests for type errors
    - Domain code free of side effects

## Functional Statically Typed Web

* Many languages, many frameworks!
* Look for the patterns and safety
* Less power is more power
* Today's focus is Haskell

# Writing Web Applications with Haskell

## Underpinnings

* Web Application Interface (WAI)
    - Common interface betwen web applications and web servers
    - Mix frameworks in one application
    - Comparable with Java Servlet API
* Warp
    - WAI web server
    - Uses GHC's lightweight threads

## Frameworks

* Scotty
* Spock
* Yesod
* Happstack
* Snap
* Airship
* Servant
* MFlow

# Scotty

## Scotty

* Inspired by Ruby's Sinatra
* Features
    - Routing and parameters
    - Web server setup
    - Monad transformer
* "Build your own framework"

## Scotty Routing

``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=app}
```

## Scotty Server

``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=main}
```

## HTML Templates

``` {.haskell}
let bootstrapCss = "https://maxcdn.bootstrapcdn.com/..."

get "/greet-with-template/:who" $ do
  who <- param "who"
  html $
       "<!DOCTYPE html>\
       \<html lang=\"en\">\
       \<head>\
       \  <meta charset=\"UTF-8\">\
       \  <title>My Page</title>\
       \  <link rel=\"stylesheet\"\
       \        href=" <> bootstrapCss <> "\">\
       \</head>\
       \<body>\
       \  <div class=\"jumbotron\">\
       \     <h1>Hello, " <> who <> "!</h1>\
       \  </div>\
       \</body>\
       \</html>\
       \"
```

## HTML Template Error!

\begin{textblock*}{0cm}(2.5cm,4.6cm)
\begin{tikzpicture}
\node at(0, 0) [draw, stringcolor,line width=3pt,ellipse, minimum width=80pt, minimum height=20pt]{};
\end{tikzpicture}
\end{textblock*}

``` {.haskell}
let bootstrapCss = "https://maxcdn.bootstrapcdn.com/..."

get "/greet-with-template/:who" $ do
  who <- param "who"
  html $
       "<!DOCTYPE html>\
       \<html lang=\"en\">\
       \<head>\
       \  <meta charset=\"UTF-8\">\
       \  <title>My Page</title>\
       \  <link rel=\"stylesheet\"\
       \        href=" <> bootstrapCss <> "\">\
       \</head>\
       \<body>\
       \  <div class=\"jumbotron\">\
       \     <h1>Hello, " <> who <> "!</h1>\
       \  </div>\
       \</body>\
       \</html>\
       \"
```

## DSLs for HTML

* Instead of HTML in strings, we use DSLs
* Embedded:
    - Blaze
    - Lucid
* External:
    - Heist
    - Hamlet
* Type safety
* Composable

## Lucid HTML Template

``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=lucid-template}
```

## Rendering Lucid with Scotty

``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=lucid-handler dedent=2}
```

## Result

![](../../src/lucid-hello.png){width=75%}

## Side Effects in Scotty

* So far we haven't done more than sending HTML responses
* We want to do IO, e.g. talk to a database
* IO can be done in Scotty handlers using `liftIO`

## IO in Scotty

* As an example, say we have this definition:

    ``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=addNewComment}
    ```
* We can use it in a handler:

    ``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=post-handler dedent=2}
    ```


## Starting with Scotty

* Easy to get started, learn the basics
* What you don't get:
    - Templating
    - Sessions
    - Authentication and Authorization
    - Logging
    - Persistence
* Have a look at Spock\fnote{\url{https://www.spock.li}} for more features

# Yesod

## Yesod

* "One-stop shop" for Haskell web development
    - A framework
    - Batteries included
    - Still very modular
* Also runs on WAI

## Batteries Included with Yesod

* Type-safe routing
* External templates for HTML, CSS, and Javascript
* Widgets
* Forms
* Sessions
* Integration with Persistent
* Authentication and Authorization
* Internationalization
* Logging
* Configuration
* Auto-reloading web server

## Getting Started

* Use a template (see `stack templates`)
* There will be things you don't understand at first
* Start out exploring:
    - Routing
    - Templates (HTML, CSS, Javascript)
    - The "Foundation" type
    - Getting something done!
* Over time, you'll understand the scaffolding
* Use the auto-reloading web server
    - Install `yesod-bin`
    - Run `yesod devel`

## Routes Configuration

``` {.changelog include=src/listings/yesod-demo/config/routes snippet=routes}
```

## A Simple Handler

``` {.haskell include=src/listings/yesod-demo/src/Handler/Home.hs snippet=get-home-handler}
```

## Hamlet Template

``` {.hamlet include=src/listings/yesod-demo/templates/homepage.hamlet}
```

## Routing with Path Pieces

``` {.haskell include=src/listings/yesod-demo/src/Handler/Home.hs snippet=get-post-handler}
```

## Post Hamlet Template

``` {.hamlet include=src/listings/yesod-demo/templates/post.hamlet}
```

## Widgets

* Reusable components of HTML, CSS, and Javascript
* We used widgets in handlers:

	```{.haskell}
  $(widgetFile "post")
	```

* Yesod tries to find matching widget files:

	```{.changelog}
	templates/post.hamlet
	templates/post.cassius
	templates/post.lucius
	templates/post.julius
	```

* Can refer to bindings in Haskell code
* Only include small parts, or use external resources

\notelist {
	\item External resources can be referred with type safe URLs
}

## Lucius (CSS Templates)

``` {.css include=src/listings/yesod-demo/templates/post.lucius}
```

## Home Page Result

![](../../src/yesod-home.png){width=75%}

## Post Page Result

![](../../src/yesod-post.png){width=75%}

## Yesod Forms

* Write forms using applicative or monadic style
* Use the same structure for rendering, parsing, and validation
* There are various renderers available

## Comment Form

``` {.haskell include=src/listings/yesod-demo/src/Handler/Home.hs snippet=comment-form}
```

## Rendering a Form

``` {.haskell include=src/listings/yesod-demo/src/Handler/Home.hs snippet=get-post-with-form-handler}
```

## Parsing and Validating the Form

``` {.haskell include=src/listings/yesod-demo/src/Handler/Home.hs snippet=post-comment-handler}
```

## Comment Form Result

![](../../src/yesod-form.png){width=75%}

## Yesod Recap

* Very capable, hit the ground running
* We only looked at some core features
* Worth learning

# Airship

## Airship

* Inspired by Webmachine from Erlang
* Define RESTful resources
* Override fields in the default resource
* Tie together resources with routing

## Airship Routes

``` {.haskell include=src/listings/airship-demo/src/Main.hs snippet=app-routes}
```

## Defining Resources

``` {.haskell}
postResource :: Resource IO
postResource =
  defaultResource
  {
	-- overrides ...
  }
```

## resourceExists

``` {.changelog}
...
```

``` {.haskell include=src/listings/airship-demo/src/Main.hs snippet=resourceExists}
```

``` {.changelog}
...
```

## contentTypesProvided

``` {.changelog}
...
```

``` {.haskell include=src/listings/airship-demo/src/Main.hs snippet=contentTypesProvided}
```

``` {.changelog}
...
```

## 404 Not Found

``` bash
$ curl -i 'localhost:3000'
HTTP/1.1 404 Not Found
Transfer-Encoding: chunked
Date: Tue, 12 Dec 2017 15:43:29 GMT
Server: Warp/3.2.13
Content-Type: text/html

Not found!
```

## 405 Method Not Allowed

``` bash
$ curl -i -X PUT 'localhost:3000/posts/1'
HTTP/1.1 405 Method Not Allowed
Transfer-Encoding: chunked
Date: Tue, 12 Dec 2017 15:44:21 GMT
Server: Warp/3.2.13
Allow: GET,HEAD,POST
```

## 406 Not Acceptable

``` bash
$ curl -i -H 'Accept: text/plain' 'localhost:3000/posts/1'
HTTP/1.1 406 Not Acceptable
Transfer-Encoding: chunked
Date: Tue, 12 Dec 2017 15:48:27 GMT
Server: Warp/3.2.13
```

## 200 OK

``` bash
$ curl -i 'localhost:3000/posts/1'
HTTP/1.1 200 OK
Transfer-Encoding: chunked
Date: Tue, 12 Dec 2017 15:45:29 GMT
Server: Warp/3.2.13
Content-Type: text/html

<h1>Airship Webmachines!</h1><p>Lorem ipsum...</p>
```

## Overrides

```{.changelog}
allowMissingPost           lastModified
allowedMethods             languageAvailable
contentTypesAccepted       malformedRequest
contentTypesProvided       movedPermanently
deleteCompleted            movedTemporarily
deleteResource             multipleChoices
entityTooLarge             previouslyExisted
forbidden                  processPost
generateETag               resourceExists
implemented                serviceAvailable
isAuthorized               uriTooLong
isConflict                  validContentHeaders
knownContentType
```

## Airship Considerations

* It is more low-level/barebones
* Again, "build your own framework"
* Suited for RESTful APIs

# Client-Side Technologies

## Client-Side Technologies

Need to do a single-page app?

* PureScript, Elm, TypeScript, GHCJS, Scala.js, Fable JS, etc
* Consider Haskell for your backend
* With Servant, you can use `servant-purescript` or `servant-elm`

# Summary

##

\centering{
  \includegraphics[width=5cm]{../../src/hammer.png}
  \vspace{.5cm}
  \Large{Reduce fear in evolving software\\by using stronger and safer tools.}
}

##

\centering{
  \includegraphics[width=5cm]{../../src/tokens.jpeg}
  \vspace{.5cm}
  \Large{Spend your complexity budget carefully.}
}

##

\centering{
  \includegraphics[width=2cm]{../../src/mwa-logo.png}
  \vspace{.5cm}
  \Large{Explore the wonderful world of functional\\and statically typed server-side web.}
}

## Links

* Slides and code: ...
* Website: [https://wickstrom.tech](https://wickstrom.tech)
* Twitter: [\@owickstrom](https://twitter.com/owickstrom)

# Questions?
