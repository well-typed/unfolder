# Haskell Unfolder episode 53: Static pointers

"Static pointers" are references to statically known values, and can serialized
independent of the type of the value (even if that value is a function), so that
you can store them in files, send them across the network, etc. In this episode
we discuss how static pointers work, and we show how we can use the primitive
building blocks provided by `ghc` to implement a more compositional interface.
We also briefly discuss how the rules for static pointers will change in ghc
9.14.2 and later.

## References

- "Towards Haskell in the Cloud", Jeff Epstein et al., Haskell 2011
  https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/remote.pdf

- GHC manual section 6.15.3, "Static pointers"
  https://downloads.haskell.org/ghc/latest/docs/users_guide/exts/static_pointers.html

- Documentation for `GHC.StaticPtr`
  https://hackage-content.haskell.org/package/base-4.22.0.0/docs/GHC-StaticPtr.html

## Important applications of static pointers

- Cloud Haskell
  https://hackage.haskell.org/package/distributed-process

  Uses static pointers to spawn processes on other nodes in the network.

- The new Cabal hooks infrstructure
  https://hackage-content.haskell.org/package/Cabal-hooks-3.16/docs/Distribution-Simple-SetupHooks.html

  Uses static pointers in a very similar way as we demonstrate in this episode.
  Cabal Hooks can register "preprocessors" for certain files, which might then
  be invoked when needed (for example by HLS). When a preprocessor is needed,
  the setup script is invoked with command line arguments that tell it to "run
  preprocessor such-and-such on files this-and-that". Rather than using
  arbitrary labels (strings) for the preprocessors, the hooks API uses static
  pointers, so that the _author_ of hooks does not need to provide a mapping
  from labels to functions, but can just directly write the code to be
  executed--as long as it is static.
