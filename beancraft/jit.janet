# JIT Compiler for Beancraft
#
# Compiles beancraft programs to native Janet code for faster execution.
# This eliminates interpreter overhead:
# - No hash table lookups for registers (uses local variables)
# - No bounds checking per instruction (pre-validated)
# - No case dispatch overhead (direct jumps via case)

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

(defn jit-compile
  "Compile a beancraft program to an executable Janet function.

   Returns a function that takes [max-steps] and returns
   {:registers table :steps number :halted boolean}"
  [program]
  (let [[code _] (compile-to-janet program)]
    (eval code)))

(defn jit-run
  "Compile and run a beancraft program using JIT compilation.

   Options:
   - max-steps: Maximum steps before stopping (default: 10,000,000)

   Returns {:registers table :steps number :halted boolean}"
  [program &opt max-steps]
  (default max-steps DEFAULT-JIT-MAX-STEPS)
  (let [compiled-fn (jit-compile program)]
    (compiled-fn max-steps)))

(defn show-generated-code
  "Show the generated Janet code for debugging/inspection."
  [program]
  (let [[code _] (compile-to-janet program)]
    (string/format "%j" code)))
