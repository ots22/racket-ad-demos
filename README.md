# rackpropagator
Exploring Automatic Differentiation in Racket

## Getting started

The code in this repository depends on a working Racket installation.
To get started with Racket, see (TODO link).

The examples in this repository can all be run locally.  To install
the package in the root of your 

You can also
`raco`

## Contents

This repository contains several implementations of automatic
differentiation algorithms, in varying states of completeness and
robustness.

## Dual numbers

The is a straightfoward dual-numbers

Known issues: it uses a single dual part, and so suffers from
perturbation confusion in certain circumstances.

```racket
#require
```


## Continuations

This is currently a single example, illustrating how reverse-mode AD
can be implemented using a local program transformation, with
continuations.  It is based on (TODO citation).

...

