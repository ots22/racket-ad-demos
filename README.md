![raco test](https://github.com/ots22/racket-ad-demos/workflows/raco%20test/badge.svg)

# racket-ad-demos
Exploring Automatic Differentiation in Racket

## Getting started

The code in this repository depends on a working Racket installation.
To get started with Racket, see https://racket-lang.org

To install the package, run (in this directory):

`raco pkg install`

You can also run
`raco pkg install http://github.com/ots22/racket-ad-demos`
to fetch the package and install it directly from GitHub.

## Contents

This repository contains implementations of a few [automatic
differentiation](https://en.wikipedia.org/wiki/Automatic_differentiation)
algorithms.

## Dual numbers

This is a straightfoward implementation of AD with dual-numbers.

```racket
#lang racket
(require ad/dual-numbers)

(define (f x y)
  (+ (* x x) (* x y)))

(f 10.0 2.0) ; => 120.0
((D/f f) 10.0 2.0) ; => #(22.0 10.0)
```

### Known limitations
The implementation uses a single dual part, and so suffers from
perturbation confusion in certain circumstances.


## Program tracing

```racket
#lang ad/trace
(require ad/trace/diff)

;; ...
```

### Known limitations

Program tracing has a large overhead. Each primitive operation does
quite a lot of extra work to manage the trace.

## Straightline

Uses the same algorithm (and code) as for finding a derivative via a
program trace, but at expansion time, and only on "straight line"
code.

## Continuations

This is currently a single example, illustrating how reverse-mode AD
can be implemented using dual numbers and continuations (with the
shift/reset higher-order control operators).  It is based on [Wang et
al (2018)](https://arxiv.org/abs/1803.10228).


## Additional Material

The slides for the presentation at
[Lambda Days 2020](https://www.lambdadays.org/lambdadays2020) ([video](https://youtu.be/treqJHnl7lY))
are available as
[a pdf](https://github.com/ots22/racket-ad-demos/tree/master/talk/slides-lambda-days.pdf)
or as
[a Racket slideshow program](https://github.com/ots22/racket-ad-demos/tree/master/talk/slides-lambda-days.rkt).

# License

We follow the lead of the main Racket license:
This code is distributed under the MIT license and the Apache version 2.0 license, at your option. 
