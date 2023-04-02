# Minori.Slices

Immutable memory block for F#

## Introduction

`Slice` is an immutable memory block like the `ReadOnlyMemory<'T>` but more useful as a collection.
Except for immutability, `Slice` is a port of the Go language slice to dotnet.

## Features

* Fast random access
* Fast slicing using `s[x..y]` syntax
* `slice` computation syntax for creating slices
* `Slice.map` , `Slice.filter` and more combinators
* Serialization using `JsonSerializer` (>= .NET6)
* `AsSpan()` and `AsMemory()`

## Operations

### Create

```fsharp
let s = slice { 1; 2; 3 }
let s = Slice.ofArray [| 1; 2; 3 |]
let s = Slice.empty
let s = Slice.create<int> 10 // create an empty slice with capacity 10
```

### Add items

```fsharp
let s1 = Slice.create 5                           // { }
let s2 = s1 |> Slice.add 'a'                      // { 'a' }
let s3 = s2 |> Slice.addRange [| 'b'; 'c' |]      // { 'a'; 'b'; 'c' }
let s4 = s3 |> Slice.addRange [| 'd'; 'e'; 'f' |] // { 'a'; 'b'; 'c'; 'd'; 'e'; 'f' }
let s5 = s1 |> Slice.add '%'                      // { 'a'; '%' }
```

Here, `s1`, `s2`, and `s3` reference to the same memory address.  
`s4` references the newly allocated memory becase exceeded its capacity.  
`s5` references the newly allocated memory becase `'b'` has already been written to the memory.

### Slice operation

Slice operation of `Slice` is **end-exclusive**.
So the slice returned by `s[x..y]` does **not** contain the y-th element.


```fsharp
let s1 = slice { 0; 1; 2; 3; 4 }
let s2 = s1[..3]  // { 0; 1; 2 }
let s3 = s1[3..]  // { 3; 4 }
let s4 = s1[1..3] // { 1; 2 }
```
