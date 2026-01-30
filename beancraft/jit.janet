# JIT Compiler for Beancraft
#
# Compiles beancraft programs to native Janet code for faster execution.
# This eliminates interpreter overhead:
# - No hash table lookups for registers (uses local variables)
# - No bounds checking per instruction (pre-validated)
# - No case dispatch overhead (direct jumps via case)
#
# With optimization enabled, common loop patterns are detected and replaced
# with O(1) operations:
# - Transfer loops (deb A; inc B) become: B += A; A = 0
# - Clear loops (deb A; loop) become: A = 0
# - Add patterns become: Out += A + B; A = 0; B = 0

(use ./optimize)

(def DEFAULT-JIT-MAX-STEPS 10_000_000)

(defn- safe-symbol
  "Convert a register name to a safe Janet symbol.
   Handles names like 'add-0/A' by replacing invalid chars."
  [name]
  (if (keyword? name)
    (symbol (string "_" name))
    (-> (string name)
        (string/replace-all "/" "_")
        (string/replace-all "-" "_")
        symbol)))

(defn- generate-reg-init
  "Generate variable initialization for a register."
  [reg-name value sym-map]
  (def sym (sym-map reg-name))
  ~(var ,sym ,value))

(defn- generate-instruction
  "Generate Janet code for a single instruction."
  [idx inst sym-map]
  (let [[op reg a b] inst
        reg-sym (sym-map reg)]
    (case op
      :inc ~(,idx (do (++ ,reg-sym) (set _pc ,a)))
      :deb ~(,idx (if (> ,reg-sym 0)
                    (do (-- ,reg-sym) (set _pc ,b))
                    (set _pc ,a)))
      :end ~(,idx (set _halted true))
      # Default: treat as halt
      ~(,idx (set _halted true)))))

(defn- generate-result-table
  "Generate code to build the result register table."
  [registers sym-map]
  (def pairs @[])
  (eachp [name _] registers
    (array/push pairs name)
    (array/push pairs (sym-map name)))
  ~(table ,;pairs))

(defn compile-to-janet
  "Compile a beancraft program to Janet code.

   Returns a tuple of [code sym-map] where:
   - code is Janet source that can be evaluated
   - sym-map maps register names to their symbols

   The generated function signature is:
   (fn [max-steps] {:registers table :steps number :halted boolean})"
  [program]
  (let [{:instructions instructions :registers registers} program

        # Create symbol mapping for all registers
        sym-map (tabseq [name :keys registers]
                  name (safe-symbol name))

        # Generate variable initializations
        var-inits (seq [[name val] :pairs registers]
                    (generate-reg-init name val sym-map))

        # Generate case branches for each instruction
        cases (seq [i :range [0 (length instructions)]]
                (generate-instruction i (instructions i) sym-map))

        # Flatten cases for the case statement
        flat-cases (mapcat tuple cases)

        # Generate result table construction
        result-expr (generate-result-table registers sym-map)]

    [~(fn [max-steps]
        # Initialize registers as local variables
        ,;var-inits

        # Program counter and state
        (var _pc 0)
        (var _halted false)
        (var _steps 0)

        # Main execution loop
        (while (and (not _halted) (< _steps max-steps))
          (++ _steps)
          (case _pc
            ,;flat-cases
            # Default case: halt on invalid PC
            (set _halted true)))

        # Return results
        {:registers ,result-expr
         :steps _steps
         :halted _halted})
     sym-map]))

(defn compile-to-janet-optimized
  "Compile a beancraft program to optimized Janet code.

   Detects loop patterns and replaces them with O(1) operations.
   Returns a tuple of [code sym-map analysis] where:
   - code is Janet source that can be evaluated
   - sym-map maps register names to their symbols
   - analysis contains detected optimization opportunities"
  [program]
  (let [{:instructions instructions :registers registers} program

        # Analyze the program for optimization opportunities
        analysis (analyze-program program)
        opt-starts (get-optimized-starts analysis)

        # Create symbol mapping for all registers
        sym-map (tabseq [name :keys registers]
                  name (safe-symbol name))

        # Generate variable initializations
        var-inits (seq [[name val] :pairs registers]
                    (generate-reg-init name val sym-map))

        # Generate case branches, using optimizations where available
        cases (seq [i :range [0 (length instructions)]]
                (if-let [opt-info (get opt-starts i)]
                  # This instruction starts an optimized pattern
                  (let [[opt-code _] (generate-optimized-instruction
                                       i instructions analysis sym-map)]
                    (if opt-code
                      ~(,i ,opt-code)
                      (generate-instruction i (instructions i) sym-map)))
                  # Regular instruction
                  (generate-instruction i (instructions i) sym-map)))

        # Flatten cases for the case statement
        flat-cases (mapcat tuple cases)

        # Generate result table construction
        result-expr (generate-result-table registers sym-map)]

    [~(fn [max-steps]
        # Initialize registers as local variables
        ,;var-inits

        # Program counter and state
        (var _pc 0)
        (var _halted false)
        (var _steps 0)

        # Main execution loop
        (while (and (not _halted) (< _steps max-steps))
          (++ _steps)
          (case _pc
            ,;flat-cases
            # Default case: halt on invalid PC
            (set _halted true)))

        # Return results
        {:registers ,result-expr
         :steps _steps
         :halted _halted})
     sym-map
     analysis]))

(defn jit-compile
  "Compile a beancraft program to an executable Janet function.

   Options:
   - optimize: Enable loop optimizations (default: true)

   Returns a function that takes [max-steps] and returns
   {:registers table :steps number :halted boolean}"
  [program &opt optimize]
  (default optimize true)
  (let [[code _ _] (if optimize
                     (compile-to-janet-optimized program)
                     (let [[c s] (compile-to-janet program)] [c s nil]))]
    (eval code)))

(defn jit-run
  "Compile and run a beancraft program using JIT compilation.

   Options:
   - max-steps: Maximum steps before stopping (default: 10,000,000)
   - optimize: Enable loop optimizations (default: true)

   Returns {:registers table :steps number :halted boolean}"
  [program &opt max-steps optimize]
  (default max-steps DEFAULT-JIT-MAX-STEPS)
  (default optimize true)
  (let [compiled-fn (jit-compile program optimize)]
    (compiled-fn max-steps)))

(defn show-generated-code
  "Show the generated Janet code for debugging/inspection."
  [program &opt optimize]
  (default optimize true)
  (let [[code _ _] (if optimize
                     (compile-to-janet-optimized program)
                     (let [[c s] (compile-to-janet program)] [c s nil]))]
    (string/format "%j" code)))

(defn show-optimizations
  "Show what optimizations were detected in the program."
  [program]
  (let [analysis (analyze-program program)
        loops (analysis :loops)
        multi-transfers (get analysis :multi-transfers @[])
        adds (analysis :adds)
        copies (get analysis :copies @[])]
    (print "Optimization Analysis:")
    (printf "  Instructions: %d" (analysis :instruction-count))
    (printf "  Simple loops: %d" (length loops))
    (printf "  Multi-transfers: %d" (length multi-transfers))
    (printf "  Add patterns: %d" (length adds))
    (printf "  Copy patterns: %d" (length copies))
    (print)

    (when (> (length copies) 0)
      (print "Copy patterns (preserves source):")
      (each copy copies
        (printf "  [%d] Copy: %s -> %s (using tmp: %s, exit: %d)"
               (copy :start) (copy :src-reg) (copy :dst-reg)
               (copy :tmp-reg) (copy :exit))))

    (when (> (length adds) 0)
      (print "Add patterns:")
      (each add adds
        (printf "  [%d] Add: %s + %s -> %s (exit: %d)"
               (get-in add [:first-loop :start])
               (get-in add [:src-regs 0])
               (get-in add [:src-regs 1])
               (add :dst-reg)
               (add :exit))))

    (when (> (length multi-transfers) 0)
      (print "Multi-transfers:")
      (each mt multi-transfers
        (printf "  [%d] Transfer: %s -> %s (exit: %d)"
               (mt :start) (mt :src-reg)
               (string/join (map string (mt :dst-regs)) ", ")
               (mt :exit))))

    (when (> (length loops) 0)
      (print "Simple loops:")
      (each loop loops
        (case (loop :type)
          :transfer (printf "  [%d] Transfer: %s -> %s (exit: %d)"
                           (loop :start) (loop :src-reg) (loop :dst-reg) (loop :exit))
          :clear (printf "  [%d] Clear: %s (exit: %d)"
                        (loop :start) (loop :reg) (loop :exit)))))

    analysis))
