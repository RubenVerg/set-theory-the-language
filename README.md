# Set Theory: The Language

## Description

In Set Theory: The Language (STTL), everything is a set. Natural numbers are encoded as sets, integers are, pairs are, booleans are, et cetera (okay, no "et cetera" yet, these are all the supported types)

Along with basic set notation like `∅` for an empty set or `{el1, el2, el3}`, and set operations like `∪`, `∩`, `∖`, `×` (cartesian product), `⊆`, `⊇`, `∈`, `∋`, `#` (count), all other operations are provided by *universes*. To specify a suffix, you use a blackboard bold letter after a symbol. The currently supported universes are `ℕ` for natural numbers and `ℤ` for integers.

The following operations support universes: `+`, `-`, `×`. Not all universes necessarily support all operations, for example there is no `-ℕ`.

Also available is `;` for creating pairs and `get`, which reads a set from stdin, as well as `get𝕦` (where `𝕦` is a supported universe) for parsing inside the universe.

Number literals also work with universes: `3ℕ` creates the natural 3 and `¯2ℤ` the integer -2.

Biverses also exist: these describe relations between two universes. `→` converts between universes: `→ℕℤ` converts naturals to integers, and `→ℤℕ` converts integers to naturals (where possible).

What is described so far are expressions. While expressions can constitute statements of their own, there are two more statements: `print` statements, which print the expression as a set, and `print𝕦`, which print in the context of the universe `𝕦`, where applicable.

## Running

Make sure you have GHC 9.10 and some recent version of Cabal installed.

To open a REPL, run `cabal run`. Working with files isn't supported yet.
