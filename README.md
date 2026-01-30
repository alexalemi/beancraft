# Beancraft

A register machine compiler and execution environment written in Janet.

Register machines are arguably the simplest model of universal computation. They consist of an unbounded number of **bins** that can hold an unbounded number of **beans**. Only three operations are permitted:

1. **Halt** - Stop all computation
2. **Increment** - Add one bean to a designated bin
3. **Decrement or Branch** - Remove one bean from a bin if it has beans, otherwise follow an alternative instruction

Despite this simplicity, register machines are Turing-complete - anything your computer can do, a register machine can do too.

## Installation

Requires [Janet](https://janet-lang.org/) and [Spork](https://github.com/janet-lang/spork).

```bash
# Clone the repository
git clone https://github.com/alexalemi/beancraft
cd beancraft

# Run a program
janet -m beancraft examples/add.bc A=10 B=15
```

## Usage

```bash
# Basic execution
beancraft <program.bc> [REG=VALUE ...]

# With JIT compilation (faster)
beancraft -j examples/mul.bc A=100 B=100

# Show help
beancraft --help
```

### Command Line Options

| Option | Short | Description |
|--------|-------|-------------|
| `--jit` | `-j` | Use JIT compilation for faster execution |
| `--verbose` | `-v` | Show execution statistics (steps, time) |
| `--quiet` | `-q` | Don't print the compiled program |
| `--dry-run` | `-n` | Compile only, don't execute |
| `--list-registers` | `-l` | Show all registers and exit |
| `--max-steps N` | `-s` | Set maximum execution steps |
| `--show-jit` | | Show generated JIT code |
| `--show-optimizations` | `-O` | Show detected loop optimizations |
| `--show-paths` | `-P` | Show module search paths |
| `--no-optimize` | | Disable JIT loop optimizations |
| `--bignum` | `-b` | Use arbitrary-precision integers in JIT mode |

### Examples

```bash
# Run addition with verbose output
beancraft -v examples/add.bc A=10 B=15

# Run multiplication with JIT (much faster for large values)
beancraft -j -v examples/mul.bc A=1000 B=1000

# See what optimizations are detected
beancraft -O examples/mul.bc

# List available registers
beancraft -l examples/copy.bc
```

## Language Grammar

Programs consist of labeled instructions:

```
# Comments start with #

# Labels are optional, end with colon
label: instruction

# Instructions:
inc REG [next]      # Increment register, jump to next (or following instruction)
deb REG jump [next] # If REG > 0: decrement and goto next; else goto jump
end                 # Halt execution

# Short forms:
+ REG [next]        # Same as inc
- REG jump [next]   # Same as deb
.                   # Same as end
```

### Special Keywords

| Keyword | Meaning |
|---------|---------|
| `self` | Current instruction |
| `next` | Next instruction |
| `prev` | Previous instruction |
| `done` | End of current block |
| `halt` / `end` | Halt instruction |
| `init` | First instruction |
| `+N` / `-N` | Jump N instructions forward/back |

### Module System

Import other beancraft files with `use`:

```
# Import a module
use "copy" From=A To=B

# With explicit scope
use "add":myadd A=x B=y

# Module search paths (in order):
# 1. Same directory as importing file
# 2. BEANCRAFT_PATH environment variable
# 3. ~/.beancraft/lib/
# 4. BEANCRAFTROOT
```

Set custom search paths:
```bash
export BEANCRAFT_PATH="/my/modules:/shared/lib"
```

## JIT Compiler

The JIT compiler translates beancraft programs to native Janet code, providing significant speedup:

```bash
# Compare interpreter vs JIT
beancraft -v examples/mul.bc A=100 B=100      # Interpreter
beancraft -j -v examples/mul.bc A=100 B=100   # JIT (much faster)
```

### Loop Optimizations

The JIT detects common patterns and replaces O(n) loops with O(1) operations:

| Pattern | Before | After |
|---------|--------|-------|
| Transfer | `deb A; inc B; loop` | `B += A; A = 0` |
| Clear | `deb A; loop` | `A = 0` |
| Add | Two transfers to same target | `Out += A + B` |
| Copy | Transfer + restore | `B += A` (A preserved) |

This means multiplication (`mul.bc`) runs in near-constant time regardless of input values.

## Example Programs

| File | Description |
|------|-------------|
| `add.bc` | Add two numbers: `Out = A + B` |
| `copy.bc` | Copy preserving source: `To += From` |
| `mul.bc` | Multiply: `Out = A * B` |
| `div.bc` | Integer division |
| `divmod.bc` | Division with remainder |
| `dayOfWeek.bc` | Calculate day of week (Sakamoto's algorithm) |

### Simple Addition (add.bc)

```
# Add A and B into Out
init: deb A copyB
      inc Out prev

copyB: deb B done
       inc Out prev
```

### Copy with Preservation (copy.bc)

```
# Copy From to To without destroying From
deb tmp loop self
loop: deb From refill
      inc To
      inc tmp loop
refill: deb tmp done
        inc From prev
```

## Project Structure

```
beancraft/
  parse.janet     # Parser and compiler
  env.janet       # Interpreter
  jit.janet       # JIT compiler
  optimize.janet  # Loop pattern detection
  loader.janet    # Module loader
  bignum.janet    # Arbitrary precision integers
  init.janet      # CLI entry point
examples/         # Example programs
test/             # Test suite
```

## Future Work

- Multiplication pattern detection (nested loops)
- Bignum optimizations in JIT (currently unoptimized)
- More example programs (GCD, prime checker, etc.)
- Standard library of common operations

## License

MIT
