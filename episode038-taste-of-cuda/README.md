# Haskell Unfolder episode 38: Tasting and testing CUDA (map, fold, scan)

CUDA is an extension of C for programming NVIDIA GPUs. In this episode of the
Haskell Unfolder we show how to set up a CUDA library so that we can link to it
from a Haskell application, how we can call CUDA functions from Haskell, and how
we can use QuickCheck to find subtle bugs in our CUDA code. On the CUDA side, we
show how to implement simple concurrent versions of `map`, `fold` and `scan`. No
familiarity with CUDA will be assumed, but of course we will only be able to
give a taste of CUDA programming.

## Requirements

Assumed that you have ghc 9.2 or higher, a CUDA compatible GPU, are using NVIDIA
driver, and have the appropriate CUDA toolchain installed. To check, verify the
version of

```bash
nvcc --version
```

and compare this to the output of

```bash
nvidia-smi
```

The CUDA version supported by the driver must equal or exceed the version of
`nvcc`.

## Local setup

First build the C(UDA) component by running `make` in `cuda/`.

To build and run the Haskell application, we need to make sure all paths are
setup correctly:

* The `episode038-taste-of-cuda/cuda/` directory needs to be in your
  `PKG_CONFIG_PATH` (to build the application) as well as your `LD_LIBRARY_PATH`
  (to run it). If you are using `direnv`, this can be done by creating a
  `haskell/.envrc` file containing

  ```bash
  export PKG_CONFIG_PATH=$PWD/../cuda:$PKG_CONFIG_PATH
  export LD_LIBRARY_PATH=$PWD/../cuda:$LD_LIBRARY_PATH
  ```

* If your CUDA libraries are installed in a non-standard location, you might
  need to create a `cabal.project.local` file. For example:

  ```
  package ep38
    extra-lib-dirs: /usr/local/cuda-12.5/lib64
  ```

## Shortcuts

To keep the CUDA code as simple as possible, we have taken some shortcuts:

1. We omit all CUDA error checking.
2. We assume all arrays are have a maximum of 1024 elements.

For a more complete version of vector addition, see the sample from the NVIDIA
itself at https://github.com/NVIDIA/cuda-samples/blob/master/Samples/0_Introduction/vectorAdd/vectorAdd.cu .

Shortcut (1) is easy to fix in all examples, but adds a lot of boilerplate
code that distracts from the principles we wanted to highlight in this episode.

Shortcut (2) is easy to fix for vector addition (the reference above shows how),
but more difficult for the fold and scan examples, because thread
synchronization is only possible within a block (and a block can contain at most
1024 threads). A good reference is

  Programming Massively Parallel Processors: A Hands-on Approach [Fourth Ed.]
  Wen-mei W. Hwu, David B. Kirk and Izzat El Hajj

Chapter 10 "Reduction" discusses folds, and chapter 11 "Prefix sum (scan)"
discusses scans.

For more information the shuffle functions, see
https://docs.nvidia.com/cuda/cuda-c-programming-guide/#warp-shuffle-functions .