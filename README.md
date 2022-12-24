# Umi

Note that the .expected files are the expected output of the corresponding .umi files.

## Goals

-   [x] Compiles to [CIL](https://en.wikipedia.org/wiki/Common_Intermediate_Language)
-   [x] [Turing Complete](https://github.com/kowasaur/umi/blob/main/examples/rule110.umi)
-   [ ] Self-hosted
-   [ ] Published on the AUR
-   [ ] Pong
-   [ ] VSCode extension with auto-complete, etc
-   [ ] Interoperability with C#

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

### Planned

-   Aliases within local scopes
-   Macros or inline functions
-   `for` `in` loop
-   Creating range array with `...`
    -   E.g. `0...10`
-   String interpolation
-   Anonymous functions
-   Switch cases
-   Namespaces
