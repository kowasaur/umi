# Umi

Note that the .expected files are the expected output of the corresponding .umi files.

## Example

```rb
Int !(Int n, Void _) {
    if n == 0 { 1 }
    else { n * (n - 1)! }
}

Void main() {
    print(0!)  #         1
    print(1!)  #         1
    print(5!)  #       120
    print(10!) #   3628800
    print(12!) # 479001600
}
```

## Goals

-   [x] Compiles to [CIL](https://en.wikipedia.org/wiki/Common_Intermediate_Language)
-   [x] [Turing Complete](https://github.com/kowasaur/umi/blob/main/examples/rule110.umi)
-   [ ] Self-hosted
-   [ ] Published on the AUR
-   [x] [Pong](https://github.com/kowasaur/umi/tree/main/examples/pong)
-   [ ] VSCode extension with auto-complete, etc
-   [x] Interoperability with C# (can use any C# file in Umi file but have to manually make bindings)

## Features

<!-- TODO: Choose stuff that you would actually want to present -->

-   Strong static typing
-   Implicit and explicit return
-   Function overloading
-   `if` and `else` statements
-   `while` loops
-   Aliases
-   Classes
    -   All methods are virtual by default
    -   Inheritance
-   Generics
-   Immutability by default
    -   Immutable references like `final` in Java
    -   Make variables mutable with `mut` keyword
-   Including other umi files (importing)
-   Creating range array with `...`
    -   E.g. `0...10`

### Planned

-   Macros or inline functions
-   `for` `in` loop
-   String interpolation
-   Anonymous functions
-   Switch cases
