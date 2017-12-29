# haskell-opencl-examples

[![Build Status][bldstat-img]][bldstat]

Examples using the [OpenCL][opencl] package in Haskell.

## Requirements

These examples are built using the [Haskell Stack Tool][stack]. Please see their
instructions for installing stack.

For MacOS, stack should be able to fetch all dependencies itself, build
everything, and produce runnable examples. On other platforms (eg. Linux and
Windows) it may also be necessary to install an OpenCL base and an OpenCL ICD
(Installable Client Driver) before the examples can compile and run. I will add
more instructions for other platforms here once I've figured out the necessary
steps.

## Additional Documentation

I have described some of this project in a [blog post][intro-post], which may be
helpful in following the first example.

## Compiling and Running

Try the following:

```
$ stack setup
$ stack build
$ stack exec 01-hello-world
```

If everything is working correctly, you should see output similar to the following
(possibly with different devices, depending on your system):

```
* OpenCL Platform Environment *
Platform: Apple
  Device: Intel(R) Core(TM) i7-6920HQ CPU @ 2.90GHz
  Device: Intel(R) HD Graphics 530
  Device: AMD Radeon Pro 460 Compute Engine

* Results *
Input:  [-4.0,-3.0,-2.0,-1.0,0.0,1.0,2.0,3.0,4.0]
Output: [-8.0,-6.0,-4.0,-2.0,0.0,2.0,4.0,6.0,8.0]
```

[bldstat]: https://travis-ci.org/lancelet/haskell-opencl-examples
  "Build Status"
[bldstat-img]: https://travis-ci.org/lancelet/haskell-opencl-examples.svg?branch=master
  "Build Status Image"
[intro-post]: https://lancelet.github.io/posts/2017-12-26-opencl-helloworld.html
  "Getting Started with OpenCL in Haskell"
[stack]: https://docs.haskellstack.org/en/stable/README/
  "The Haskell Tool Stack"
[opencl]: https://hackage.haskell.org/package/OpenCL
  "OpenCL package link on Hackage"
