# hs-lisp
A simple functional language with a lisp-like syntax written in Haskell
(heavily inspired by Haskell)

##### What works:
- type inference
- lambda calculus
- user-defined data types (only with type variables for now)
- pattern matching

##### TODO:
- type literals
- parser errors (ExeptT transformer)
- macro system
- dependent types
- syntax subject to change
- laziness

##### Examples:
Data type definition:
```
(data (Maybe a) [Just a] [Nothing])
```

Lambdas with pattern matching:
```
(def fromJust (\(Just x).x))

(def fmap (\f (Just x). (Just (f x))))
```

Functions with multiple patterns
```
(data (Bool) [True] [False])

(def or (
  [True x]  True
  [False x] x
))

(def and (
  [True x]  x
  [False x] False
))

(def if (
  [True a b]  a
  [False a b] b
))
```
