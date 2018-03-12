# urn-eval
An "eval" implementation for Urn. Does not actually interpret code
but rather compiles it with an extremely simple public interface.

# Usage
In order to use this library, you need to `--include` the urn repository
when compiling, for example: `urn main.lisp --include ~/Projects/urn`.

After that, just import `urn-eval.lisp` like normal.

## `(eval code lib-path)`
Compiles code and returns the compiled result as a function that can be
ran. Caches automatically.
**code**: The code that needs to be compiled.
**lib-path**: The urn standard library path. For example:
`/home/casper/Projects/urn/lib`.
*Returns* The compiled function.

## `(clear-cache!)`
Clears the cache.

## `(eval-raw code lib-path)`
Same as `eval`, but without caching.
