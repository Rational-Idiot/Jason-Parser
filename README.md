## JSON Parser

A simple JSON parser built from scratch in Haskell using parser combinators.

## What it does

It just works

## Usage

```haskell
runParser jsonValue "{\"hello\": \"world\"}"
-- Just ("", JSONObject [("hello", JSONString "world")])
```

## Why?

Why not bruv
