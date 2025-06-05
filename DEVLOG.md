# DEVLOG

## 2025-05-28

I should make it so that when you import a file you can set the registers, that
would clean things up nicely.  I should work quickly towards the universal
register machine.

## 2025-05-19

Started writing the tests with judge to test out simple programs.  Looks like
right now I've disabled the recursive `compile-use` which is likely the
problem.  Should revisit that.

## 2025-05-18

Looked at this again, we already have an environment, and a parser. Maybe janet
is the way to go, as I could also compile a static executable and ship it.

Right now I should probably build a thing that will parse and run a file with
basic support.

Today I realized that I don't think I need a separate keyword for functions and
loading, I can just make each function its own file, then I just need a way to
override the names of all of the registers, or scope them or something.

    %load filename:scope reg:alias reg2:alias2

This would load the filename and give it a scope `scope` for all of its
registers, and the optional things afterward would let you alias the registers
to existing names.

Thinking about it some more, I do think the load command has to be an
instruction, cause it needs a label.

I've written a version of the parser that converts a program to a simple
unambiguous version with a few passes. I still have to implement the use, but I
think it should be relatively simple given the way I've composed things. I also
need to switch over the to the bignums.

Alright, I think I've got something that can load things, tried out some test programs
and for some of the more complicated ones I've getting strange errors.

The issue seems to be with the `done` handling, at least I think.
