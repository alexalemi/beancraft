# beancraft

Add project description here.

I want to make a nice compiler and environment for bins and beans, in Odin.

The aim of this repo is to provide an implementation of a register machine language and environment for teaching purposes.

Register machines are arguably the simplest model of universal computation, they consist of an unbounded number of bins that can hold an unbounded number of beans, the only three operations permitted are:

 1. Halt - Stop all computation.
 2. Increment - Increment the number of beans in a designated bin.
 3. Decrement or Branch - Decrement the number of beans in a designated bin if there are beans there, otherwise follow an alternative instruction.

As simple as this setup is, this is a system capable of universal computation, i.e. anything your computer can do, this register machine can do and vice versa.

To illustrate the power of this register machine, this repository implements a virtual environment and compiler for a simple register machine language.

## Programs to Create

 - doomsday algorithm
 - sin
 - cos
 - tan
 - sqrt
 - cbrt
 - universal machine
 - mandelbrot generator
 - prime checker
 - gcd


## Grammar

Let's try to design the language
