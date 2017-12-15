---
title: Fast and Fearless Evolution of Server-Side Web Applications
author: Oskar Wickström
date: |
  \includegraphics[width=3cm]{../../src/mpowered.png}
theme: Boadilla
classoption:
  - dvipsnames
  - aspectratio=169
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
  \item Today, after a high-level introduction, we'll talk about web applications
  \item Specifically, we will talk about writing webapps in Haskell
  \item I'll demonstrate some frameworks available
  \item And say a few words about client-side technologies
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
  \item There are many reasons to evolve software
  \item We will probably have ...
}

## Risks

* Evolving software, we face risks:
    - Delay
    - Error
    - Burnout
* These risks make it scary to evolve freely

\notelist{
  \item When evolving software, we risk: ...
  \item These things hurt not only ourselves, but: Customers, partners, internal relations
  \item The risks make it scary to evolve our software freely
  \item The change you want to make can be too risky
  \item I think the fear of evolving software freely has a huge impact on our systems
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

\notelist{
  \item Many of us work with the web in one way or another ... (internet)
  \item Currently, single-page apps are trendy. These work more like ...
  \item Now, you might think "What about universal webapps?"
  \item They are about initial rendering, not about transparently running the same application client-side and server-side
}

## Reminder

\centering{\Large{$\text{Newer} \not\Longrightarrow \text{Better}$}}

\notelist{
  \item This is my friendly reminder: Never does not imply better
  \item While the single-page app tech is very new and flashy, it might not be a good choice for your project
}

## Server-Side Web Applications

* Do not dismiss server-side web applications
* Progressive enhancement
* 80/20 rule
* Use client-side code where you need it!
* PJAX

\notelist{
  \item I urge you not to dismiss server-side web applications
  \item Rather, have that as a default choice
  \item You can use what's known as "Progressive Enhancement", where ...
  \item Also important to recognize: All code is not equally valuable
  \item Pages for Settings, Login, Documentation, etc
  \item Use client-side code where you need it, where you get a return on the investment
  \item If you want more snappy navigation, things like PJAX go a long way
}

## Static Typing for Server-Side Web

* Compile-time checking
    - Run-time robustness with defined behaviour
    - Use types for correct-by-construction
    - Machine-verified living documentation, communicates intent
* Safely evolve our codebase
    - Reduce fear of change throughout the codebase
    - Modify core domain, follow the errors
    - Not split by an API
* Focus tests on our domain
    - No need to write tests for type errors
    - Domain code free of side effects

\notelist{
  \item Combining server-side web with static typing, we get a lot of benefits
}

## Functional Statically Typed Web

\begin{columns}[T,onlytextwidth]
  \begin{column}{.7\textwidth}
    \begin{minipage}{\textwidth}
      \begin{itemize}
        \item Many languages, many frameworks!
        \item Look for the patterns and safety
        \item Less power is more power
        \item Today's focus is Haskell
      \end{itemize}
    \end{minipage}
  \end{column}
  \begin{column}{.3\textwidth}
    \begin{minipage}{\textwidth}
      \includegraphics[width=2.5cm]{../../src/haskell.png}
    \end{minipage}
  \end{column}
\end{columns}

# Writing Web Applications with Haskell

## Underpinnings

* Web Application Interface (WAI)
    - Common interface betwen web applications and web servers
    - Mix frameworks in one application
    - Comparable with Java Servlet API
* Warp
    - WAI web server
    - Uses GHC's lightweight threads

\notelist{
  \item The Haskell web frameworks we'll look at all build on WAI, ...
  \item Warp is a popular and fast web server for WAI
}

## Frameworks

* Scotty
* Spock
* Yesod
* Happstack
* Snap
* Airship
* Servant
* MFlow

\notelist{
  \item In the Haskell ecosystem, there are many web frameworks.
  \item This list is not exhaustive
  \item We will look at three of these frameworks: Scotty, Yesod, and Airship
}

# Scotty

## Scotty

* Inspired by Ruby's Sinatra
* Features
    - Routing and parameters
    - Web server setup
    - Monad transformer
* "Build your own framework"

\notelist{
  \item Inspired by Ruby's Sinatra
  \item It provides routing, parameters, and form parsing
  \item It is easy to get started, setting up a web server
  \item It builds in ScottyT, a monad transformer, which you can extend
  \item Scotty is very small, and if you build something bigger, you'll likely have to "build your own framework"
}

## Scotty Routing

``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=app}
```

\notelist{
  \item This is a Scotty app with two routes
  \item (explain code)
}

## Scotty Server

``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=main}
```

\notelist{
  \item This is how we run it
  \item Now, usually you need to render larger chunks of HTML
}

## HTML Templates

``` {.haskell}
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
       \</html>"
```

\notelist{
  \item Let's say we do this
  \item It is very hard to read
  \item Sure, we could refactor to separate view functions
  \item The bigger issue is that we're doing stringly-typed programming
  \item \textbf{Can anyone tell me what's wrong here?}
}

## HTML Template Error!

\begin{textblock*}{0cm}(2.5cm,4.0cm)
\begin{tikzpicture}
\node at(0, 0) [draw, stringcolor,line width=3pt,ellipse, minimum width=80pt, minimum height=20pt]{};
\end{tikzpicture}
\end{textblock*}

``` {.haskell}
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
       \</html>"
```

\notelist{
  \item We are missing an escaped double quote here
  \item The string literal's quote makes it extra hard to see
  \item So, let's not do this.
}

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

\notelist{
  \item Instead we use a markup DSL
  \item There are embedded and external DSLs for HTML
  \item Embedded means the markup is written in regular Haskell, in Haskell source files
  \item Two popular libraries are Blaze and Lucid
  \item For external HTML templating languages, we can use Heist or Hamlet
  \item The external ones are typically written in separate files, but can also be embedded using quasi-quoting
  \item These languages give is type-safe templates that are composable
  \item They help us produce valid HTML
}

## Lucid HTML Template

``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=lucid-template}
```

\notelist{
  \item Here we see the equivalent template in Lucid
  \item Elements are nested using function application
  \item Elements are juxtaposed using do notation
  \item Attributes are set using a list of pairs
  \item Notice how some functions do not take any child content
  \item `meta` and `link` in HTML are empty elements
  \item In this way, Lucid and the type system help us construct valid HTML
}

## Rendering Lucid with Scotty

``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=lucid-handler dedent=2}
```

\notelist{
  \item We can render Lucid in a handler like this
}

## Result

![](../../src/lucid-hello.png){width=75%}

\notelist{
  \item Looking at the result in a web browser, we can inspect the rendered HTML
}

## Side Effects in Scotty

* We need more than sending HTML responses
* We want to do IO:
    - Database queries
    - Logging
    - External service calls
* IO in Scotty handlers using `liftIO`

\notelist{
  \item So far, we have only sent HTML over the wire
  \item We most likely need side-effects to do something useful
  \item (read list)
  \item We use `liftIO` in Scotty handlers to do IO
}

## IO in Scotty

* Given these definitions:

    ``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=addNewComment}
    ```
* We can \textit{lift} the IO action into a handler:

    ``` {.haskell include=src/listings/haskell-examples/src/Scotty.hs snippet=post-handler dedent=2}
    ```

\notelist{
  \item Let's look at an example of using `liftIO`
  \item Given these definitions... (explain rest)
}


## Starting with Scotty

* Easy to get started, learn the basics
* What you don't get:
    - Templating
    - Sessions
    - Authentication and Authorization
    - Logging
    - Persistence
* Have a look at Spock\fnote{\url{https://www.spock.li}} for more features

\notelist{
  \item I recommend starting out with Scotty if you're new to Haskell web development
  \item Once your applications grows, you will probably need to bring in libraries
  \item For a slightly larger feature set, and type-safe routing, have a look at Spock
}

# Yesod

## Yesod

* "One-stop shop" for Haskell web development
    - A framework
    - Batteries included
    - Still very modular
* Also runs on WAI

\notelist {
  \item Yesod can be called a "one-stop shop" for Haskell web development
  \item It is a framework with many batteries included
  \item Still, it is implemented to be modular
  \item Things are there by default, but you can swap them out if you need
  \item Yesod runs on WAI using the Warp server
}

## Batteries Included with Yesod

\begin{columns}[T,onlytextwidth]
  \begin{column}{.5\textwidth}
    \begin{minipage}{\textwidth}
      \begin{itemize}
        \item Type-safe routing
        \item External templates for:
          \begin{itemize}
            \item HTML
            \item CSS
            \item Javascript (and TypeScript)
          \end{itemize}
        \item Widgets
        \item Forms
        \item Sessions
      \end{itemize}
    \end{minipage}
  \end{column}
  \begin{column}{.5\textwidth}
    \begin{minipage}{\textwidth}
      \begin{itemize}
        \item Integration with Persistent
        \item Authentication and Authorization
        \item Internationalization
        \item Logging
        \item Configuration
        \item Auto-reloading web server
      \end{itemize}
    \end{minipage}
  \end{column}
\end{columns}

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
    - Install `yesod-bin`, run `yesod devel`

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

## Airship Overrides

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
  \includegraphics[width=4cm]{../../src/brain.png}
  \Large{Evolve software fearlessly using better tools \\ for modeling and communication.}
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
