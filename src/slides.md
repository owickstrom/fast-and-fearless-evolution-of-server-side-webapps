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

## Blaze

``` {.haskell}
H.div $ do
  H.h1 "My Title"
  H.p "Yeah..."
```

# Client-Side Technologies

# Questions?

## Links

* Slides and code: ...
* Website: [https://wickstrom.tech](https://wickstrom.tech)
* Twitter: [\@owickstrom](https://twitter.com/owickstrom)
