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

* Delay
* Error
* Burnout

\notelist{
  \item Who can this hurt? Customers, partners, internal relations
}

## Tooling

* We can mitigate with better tools
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

## Server-Side Web Applications

* Progressive enhancement
* 80/20 rule
* Use client-side code where you need it!
* PJAX

## Reminder

\centering{\Large{$\text{Newer} \not\Longrightarrow \text{Better}$}}

## Static Typing for Server-Side Web

* Compile-time checking
    - Run-time robustness with defined behaviour
    - Use types for correct-by-construction
    - Machine-verified living documentation, communicates intent
* Safely evolve our codebase
    - Good tools reduce fear of change
    - Modify core domain, follow the errors
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

## Type-Safe Routing: Links

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

* Only include small amount of local styling this way!
* `addStylesheet` takes a type-safe URL

## Lucius (CSS Templates)

``` {.css include=src/listings/yesod-demo/templates/post.lucius}
```

## Home Page Result

![](../../src/yesod-home.png){width=75%}

## Post Page Result

![](../../src/yesod-post.png){width=75%}

## Forms

# Airship

## Airship

## Example Resource

# Client-Side Technologies

# Questions?

## Links

* Slides and code: ...
* Website: [https://wickstrom.tech](https://wickstrom.tech)
* Twitter: [\@owickstrom](https://twitter.com/owickstrom)
