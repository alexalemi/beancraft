# beancraft

WORK IN PROGRESS

I want to make a nice compiler and environment for bins and beans, in Janet.

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


# Grammar

The grammar takes the form:

# comment
label: use "flname":scope reg=alias reg2=alias2 label~newLabel label2~newLabel2
label: end
label: inc next
label: deb jump next

The labels are optional.

For increment, if you leave off the next, its assumed to be the next instruction.
For deb, if you leave off the next, its assumed to be the next instruction.

There are special keywords: self, next, prev, done, halt, init

For the 'next's, you can also use +n or -n, which jumps that many instructions forward or back.
