# Title

webui

# Author

Amirouche BOUBEKKI

# Status

Early Draft

# Abstract

This library describes a framework to build graphical user interface
based on `sxml`.  It takes inspiration from
[elm](https://elm-lang.org/) and [Redux](https://redux.js.org/).
Unlike elm it is not strictly functional.  It takes advantage of
diff + patch algorithm that powers virtual dom libraries such as
[React](https://reactjs.org/), [Preact](https://preactjs.com/) and
[snabbdom](https://github.com/snabbdom/snabbdom/).

# Issues

??? Optional section that may point out things to be resolved. This
will not appear in the final SRFI.

# Rationale

The main advantage of the "virtual DOM" approach to build a graphical
user interface is that the developer does not need to figure how to
mutate the scene graph, or hierarchy of widgets, or as it is the case
in the browser, the DOM nodes that must be added, updated or deleted.

The developer can focus on *what* application they want to build
instead of *how* to build the application.

The diff + patch algorithm factors all the mutation operations inside
this library making it possible to reduce non-functional code in the
application, hence it is easier to think about the application.

The approach taken in elm that is strictly functional, and that rely
on continuations (or if you prefer: callbacks) is verbose, and as
matter of fact leads to code that is more difficult to reason about.

This library bridge the gap with the excellent testability of elm
approach by passing procedures that have side-effects as arguments,
that way it is trivial to mock behaviors that have side-effects during
testing.

The fact that this library re-use the vocabulary of the most known
graphical user interface pattern ie. Model-View-Controler is not a
mistake.

# Specification

## `(webui-app patch! init view) → procedure`

`webui-app` takes the procedures `PATCH!`, `INIT` and `VIEW` and
return a procedure called "change" procedure.

Here is a minimal example use that describe the implementation of a
game where you need to click on a button to increment a counter:

```scheme
(define (on-click model event)
  (+ model 1))

(define (view model)
  `(div
    (h1 "counter is " model)
    (button (@ (on-click ,on-click)) "increment")))

(define change (webui-app webui-patch! (lambda () 0) view))
```

- `PATCH!` is a procedure that binds reactjs with scheme. It takes a
  description of the DOM using sxml that the user wants to display
  along with the controller procedures like `on-click` in the above
  example. `PATCH!` is described in the next section.
  
- `INIT` is a thunk that returns the initial model for the application.
  It should return an immutable object.
  
- `VIEW` is a procedure that takes the model as argument and return
  the description of the expected DOM.

TODO: describe single direction data-flow.

TODO: describe the behavior of controllers.

`webui-app` is really useful to explain the gist of the approach or
prototyping.  For real world web application that require calls to the
backend and url routes use `webui-app*`

## `(webui-patch! node sxml)`

## `(webui-style alist)`

## `(webui-route pattern init view)`

`VIEW` should be side-effect free, otherwise the behavior is not
specified.

## `(webui-router routes)`

## `(webui-change-route path)`

## `(webui-xhr method headers path body)`

## `(webui-app* patch! router xhr)`

# Implementation

??? explanation of how it meets the sample implementation requirement
(see process), and the code, if possible, or a link to it Source for
the sample implementation.

# Acknowledgements

Give credits where credits is due.

# Copyright

Copyright (C) Amirouche BOUBEKKI (2020).

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
