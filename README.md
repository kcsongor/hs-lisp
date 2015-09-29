# hs-lisp
A simple functional toy language with a lisp-like syntax written in Haskell
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
- type classes
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

Functions are curried (so partial application is possible):
```
>> (def f (+ 1))
'f :: (Int -> Int)
>> (f 41)
42 :: Int
```

Using tuples:
```
(def myself (, "Csongor" 19))

(def greet (
  [(, name age)] 
    (let offer (if (> 18 age) "coke" "whiskey")
      (_ ++ ["Hi " name ", would you like a " offer "?"]))))
```
(The function _ is a synonym for foldl1)

Then in the REPL:
```
>> (greet myself)
"Hi Csongor, would you like a whiskey?" :: String
```
(No thanks, I don't like whiskey)
