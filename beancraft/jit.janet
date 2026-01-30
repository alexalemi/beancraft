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
#
# Bignum mode: Use arbitrary-precision integers for registers.
# This allows computation with numbers larger than Janet's native int64.

(use ./optimize)
(use ./bignum)

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

# ============================================================
# Bignum-aware code generation
# ============================================================

(defn- generate-bignum-reg-init
  "Generate bignum variable initialization for a register."
  [reg-name value sym-map]
  (def sym (sym-map reg-name))
  ~(var ,sym (bignum/from-num ,value)))

(defn- generate-bignum-instruction
  "Generate Janet code for a single instruction using bignums."
  [idx inst sym-map]
  (let [[op reg a b] inst
        reg-sym (sym-map reg)]
    (case op
      :inc ~(,idx (do (bignum/inc ,reg-sym) (set _pc ,a)))
      :deb ~(,idx (if (not (bignum/zero? ,reg-sym))
                    (do (bignum/dec ,reg-sym) (set _pc ,b))
                    (set _pc ,a)))
      :end ~(,idx (set _halted true))
      # Default: treat as halt
      ~(,idx (set _halted true)))))

(defn- generate-bignum-result-table
  "Generate code to build the result register table, converting bignums to numbers."
  [registers sym-map]
  (def pairs @[])
  (eachp [name _] registers
    (array/push pairs name)
    (array/push pairs ~(bignum/to-num ,(sym-map name))))
  ~(table ,;pairs))

# ============================================================
# Bignum-aware optimized code generation
# ============================================================

(defn- generate-bignum-optimized-transfer
  "Generate optimized code for a transfer loop using bignums.
   Instead of looping, do: dst = dst + src; src = 0"
  [loop-info sym-map]
  (let [src-sym (sym-map (loop-info :src-reg))
        dst-sym (sym-map (loop-info :dst-reg))
        exit (loop-info :exit)]
    ~(do
       (set ,dst-sym (bignum/add ,dst-sym ,src-sym))
       (set ,src-sym (bignum/from-num 0))
       (set _pc ,exit))))

(defn- generate-bignum-optimized-clear
  "Generate optimized code for a clear loop using bignums.
   Instead of looping, do: reg = 0"
  [loop-info sym-map]
  (let [reg-sym (sym-map (loop-info :reg))
        exit (loop-info :exit)]
    ~(do
       (set ,reg-sym (bignum/from-num 0))
       (set _pc ,exit))))

(defn- generate-bignum-optimized-add
  "Generate optimized code for an add pattern using bignums.
   Instead of two transfer loops, do: dst = dst + src1 + src2; src1 = 0; src2 = 0"
  [add-info sym-map]
  (let [src1-sym (sym-map (get-in add-info [:src-regs 0]))
        src2-sym (sym-map (get-in add-info [:src-regs 1]))
        dst-sym (sym-map (add-info :dst-reg))
        exit (add-info :exit)]
    ~(do
       (set ,dst-sym (bignum/add ,dst-sym ,src1-sym))
       (set ,dst-sym (bignum/add ,dst-sym ,src2-sym))
       (set ,src1-sym (bignum/from-num 0))
       (set ,src2-sym (bignum/from-num 0))
       (set _pc ,exit))))

(defn- generate-bignum-optimized-copy
  "Generate optimized code for a copy pattern using bignums.
   Instead of multi-transfer + restore, do: dst = dst + src (src preserved)"
  [copy-info sym-map]
  (let [src-sym (sym-map (copy-info :src-reg))
        dst-sym (sym-map (copy-info :dst-reg))
        tmp-sym (sym-map (copy-info :tmp-reg))
        exit (copy-info :exit)]
    ~(do
       (set ,dst-sym (bignum/add ,dst-sym ,src-sym))
       # tmp is used internally but ends up at 0
       (set ,tmp-sym (bignum/from-num 0))
       (set _pc ,exit))))

(defn- generate-bignum-optimized-multi-transfer
  "Generate optimized code for a multi-target transfer using bignums.
   Transfer src to all destinations in one step."
  [mt-info sym-map]
  (let [src-sym (sym-map (mt-info :src-reg))
        dst-syms (map |(sym-map $) (mt-info :dst-regs))
        exit (mt-info :exit)]
    ~(do
       ,;(map (fn [dst] ~(set ,dst (bignum/add ,dst ,src-sym))) dst-syms)
       (set ,src-sym (bignum/from-num 0))
       (set _pc ,exit))))

(defn- generate-bignum-optimized-instruction
  "Generate optimized code for an instruction using bignums.
   Returns [code, skip-count] where skip-count is how many instructions to skip."
  [idx instructions analysis sym-map]
  (let [loops (analysis :loops)
        adds (analysis :adds)
        copies (analysis :copies)
        multi-transfers (analysis :multi-transfers)]

    # Check if this starts a copy pattern (highest priority - most complex)
    (var found-copy nil)
    (each copy copies
      (when (= (copy :start) idx)
        (set found-copy copy)
        (break)))

    (if found-copy
      [(generate-bignum-optimized-copy found-copy sym-map) nil]

      # Check if this starts an add pattern
      (do
        (var found-add nil)
        (each add adds
          (when (= (get-in add [:first-loop :start]) idx)
            (set found-add add)
            (break)))

        (if found-add
          [(generate-bignum-optimized-add found-add sym-map) nil]

          # Check if this starts a multi-transfer (not part of a copy)
          (do
            (var found-mt nil)
            (each mt multi-transfers
              (when (= (mt :start) idx)
                # Make sure it's not part of a copy pattern
                (var in-copy false)
                (each copy copies
                  (when (= (get-in copy [:multi-transfer :start]) idx)
                    (set in-copy true)
                    (break)))
                (unless in-copy
                  (set found-mt mt)
                  (break))))

            (if found-mt
              [(generate-bignum-optimized-multi-transfer found-mt sym-map) nil]

              # Check if this starts a simple loop
              (do
                (var found-loop nil)
                (each loop loops
                  (when (= (loop :start) idx)
                    (set found-loop loop)
                    (break)))

                (if found-loop
                  (case (found-loop :type)
                    :transfer [(generate-bignum-optimized-transfer found-loop sym-map) nil]
                    :clear [(generate-bignum-optimized-clear found-loop sym-map) nil]
                    [nil nil])
                  [nil nil])))))))))

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

(defn compile-to-janet-bignum-optimized
  "Compile a beancraft program to optimized Janet code using bignums.

   Detects loop patterns and replaces them with O(1) bignum operations.
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

        # Generate variable initializations with bignums
        var-inits (seq [[name val] :pairs registers]
                    (generate-bignum-reg-init name val sym-map))

        # Generate case branches, using optimizations where available
        cases (seq [i :range [0 (length instructions)]]
                (if-let [opt-info (get opt-starts i)]
                  # This instruction starts an optimized pattern
                  (let [[opt-code _] (generate-bignum-optimized-instruction
                                       i instructions analysis sym-map)]
                    (if opt-code
                      ~(,i ,opt-code)
                      (generate-bignum-instruction i (instructions i) sym-map)))
                  # Regular instruction
                  (generate-bignum-instruction i (instructions i) sym-map)))

        # Flatten cases for the case statement
        flat-cases (mapcat tuple cases)

        # Generate result table construction
        result-expr (generate-bignum-result-table registers sym-map)]

    [~(fn [max-steps]
        # Initialize registers as bignum local variables
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

        # Return results (convert bignums to numbers)
        {:registers ,result-expr
         :steps _steps
         :halted _halted})
     sym-map
     analysis]))

(defn compile-to-janet-bignum
  "Compile a beancraft program to Janet code using bignums (unoptimized).

   Returns a tuple of [code sym-map] where:
   - code is Janet source that can be evaluated
   - sym-map maps register names to their symbols"
  [program]
  (let [{:instructions instructions :registers registers} program

        # Create symbol mapping for all registers
        sym-map (tabseq [name :keys registers]
                  name (safe-symbol name))

        # Generate variable initializations with bignums
        var-inits (seq [[name val] :pairs registers]
                    (generate-bignum-reg-init name val sym-map))

        # Generate case branches for each instruction
        cases (seq [i :range [0 (length instructions)]]
                (generate-bignum-instruction i (instructions i) sym-map))

        # Flatten cases for the case statement
        flat-cases (mapcat tuple cases)

        # Generate result table construction
        result-expr (generate-bignum-result-table registers sym-map)]

    [~(fn [max-steps]
        # Initialize registers as bignum local variables
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

        # Return results (convert bignums to numbers)
        {:registers ,result-expr
         :steps _steps
         :halted _halted})
     sym-map]))

(defn jit-compile
  "Compile a beancraft program to an executable Janet function.

   Options:
   - optimize: Enable loop optimizations (default: true)
   - bignum: Use arbitrary-precision integers (default: false)

   Returns a function that takes [max-steps] and returns
   {:registers table :steps number :halted boolean}"
  [program &opt optimize bignum]
  (default optimize true)
  (default bignum false)
  (let [[code _ _] (cond
                     (and bignum optimize) (compile-to-janet-bignum-optimized program)
                     bignum (let [[c s] (compile-to-janet-bignum program)] [c s nil])
                     optimize (compile-to-janet-optimized program)
                     (let [[c s] (compile-to-janet program)] [c s nil]))]
    (eval code)))

(defn jit-run
  "Compile and run a beancraft program using JIT compilation.

   Options:
   - max-steps: Maximum steps before stopping (default: 10,000,000)
   - optimize: Enable loop optimizations (default: true)
   - bignum: Use arbitrary-precision integers (default: false)

   Returns {:registers table :steps number :halted boolean}"
  [program &opt max-steps optimize bignum]
  (default max-steps DEFAULT-JIT-MAX-STEPS)
  (default optimize true)
  (default bignum false)
  (let [compiled-fn (jit-compile program optimize bignum)]
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
