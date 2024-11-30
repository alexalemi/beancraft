# Bins and Beans

This is an implementation of a register machine, in janet.

In the basic language there are only three commands:

 - `inc REGISTER [next]` - Increments the count in a register and moves to next
   instruction, if missing, goes to the next instruction.
 - `deb REGISTER jump [next]` - Decrements or branches.  Decrements the count in
   a given register if present and then goes to the `next` instruction,
   otherwise jmps to the `jump` instruction.
 - `end` - Halts.


For the full grammar of the language, we also allow optional tags for each line.

Comments start with #.

```
#This program adds the tokens in A to B. 
# When the program finishes, A has the number of tokens that it started with
# and B has the sum of the tokens originally in A and B.

init: - A end
+ B init
```

Special keywords include:
 - `self` : Yourself
 - `next` : the next instruction
 - `prev` : the previous instruction.
 - `load` : used to load another module.
 - `func` : for declaring functions.
 - `init` : initial command in scope.
