# Experiment with observing the selector thunk optimization

## `swap`

This example attempts to be the simplest illustration possible, and runs
in constant space always due to the selector thunk optimization. Compile
time optimization flags are only relevant insofar that _if_ we use `fst` and
`snd`, _then_ those calls _must_ be inlined:

* `CASE`: If we don't use them, `-O0` and `-O1` both run in constant space
* `FST_SND`: If we do use them, but allow inlining, then `-O1` runs in
  constant space but `-O0` does not.
* `PREVENT_INLINE`: If we prevent ghc from inlining, neither `-O1` nor `-O1`
  run in constant space.

## `lastbeforebreak`

This is where things start to get confusing. Some remarkable results:

### `PRINT_EQ-Int-{INLINE,NOINLINE}-BESPOKE_LAST-BESPOKE_BREAK`

This compares

```haskell
sameElem :: Int -> Int -> Bool
{-# NOINLINE #-}
sameElem = (==)
```

to the same version with `INLINE`. For `NOINLINE`, this never runs in constant
space; for `INLINE`, it runs in constant space with `-O1` but not with `-O0`,
despite `-O1` _reducing_ the number of selector thunks.

Observations:

* `-O1` applies the worker/wrapper transformation and introduces a strict worker
  for `break` which returns an unboxed tuple. However, the recursive call
  becomes a bit weird:

  ```haskell
  let result = case break xs of (# before, after #) -> (before, after)
  ```

  so it re-introduces the unboxed tuple for each recursive call.

* Top-level call to `break` becomes strict in `-O1`; this explains why we have
  fewer selector thunks in the `-O1` version.

### `PRINT_EQ-{Int,Integer}-INLINE-BESPOKE_LAST-BESPOKE_BREAK`

I suspect that this is the same difference as above, somehow; note that for the
`NOINLINE` case, the difference disappears (or at least, is invisible).

### `{SEQ_EQ,PRINT_EQ}-Integer-INLINE-BESPOKE_LAST-BESPOKE_BREAK`

This compares

```haskell
print (last_before == last_after)
```

to

```haskell
last_before `seq` last_after `seq` print (last_before == last_after)
```

For `Int` this makes no difference (as one would expect), but for `Integer` it
_does_, where the `seq` version runs in constant and the other version does not
(despite the Cmm code containing _no_ selector thunks in either case).

This is true whether or not we inline `sameElem`.

### `{ONE_PRINT,TWO_PRINTS}-Int-BESPOKE_LAST-BESPOKE_BREAK`

This compares

```haskell
print (last_before, last_after)
```

to

```haskell
print last_before
print last_after
```

The one-print version does not leak with `-O1`, but does with `-O0`; the
two-prints version leaks _always_. This is true for both `Int` and `Integer`,
and whether or not we inline `sameElem`.

Again here too, the version that does not leak has _fewer_ selector thunks.

### `PRINT_EQ-Integer-PRELUDE_LAST-{PRELUDE,BESPOKE}_BREAK`

Both of these leak with -O1` but not with `-O0` for ghc 9.2, but only if we
allow `sameElem` to be inlined; and _always_ leaks with ghc 9.4.

## TODOs

* Non-moving GC