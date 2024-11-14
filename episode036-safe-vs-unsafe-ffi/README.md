# The Haskell Unfolder episode 36: concurrency and the FFI

Abstract: there are two primary ways to import C functions in Haskell: "unsafe"
and "safe". We will first briefly recap what this means: `unsafe` functions are
fast but cannot call back into Haskell, `safe` functions are much slower but
can. As we will see in this episode, however, there are many more differences
between `unsafe` and `safe` functions, especially in a concurrent setting. In
particular, `safe` functions are not always safer!
