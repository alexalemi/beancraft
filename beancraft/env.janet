# The main environment for bins and beans.

# An environment is a struct with some registers and some symbols defined.

# instructions are :inc :deb and :end
# :inc register next
# :deb register jump next
# :end

# Default maximum steps for program execution
(def DEFAULT-MAX-STEPS 10_000)

(defdyn *MAX-STEPS*)
(setdyn *MAX-STEPS* DEFAULT-MAX-STEPS)

(def empty-env
  {:instructions [] # array of instructions
   :labels {} # table from name to instruction
   :registers @{} # register name to value.
   :pointer 0 # current instruction
   :halted false}) # whether halted.

(defn step
  "Increment the environmet."
  [env]
  (let [{:instructions instructions
         :labels labels
         :registers registers
         :pointer lbl
         :halted halted} env]
    (if halted
      env
      (do
        # Validate instruction pointer is within bounds
        (unless (and (>= lbl 0) (< lbl (length instructions)))
          (errorf "Instruction pointer %d out of bounds (0-%d)" lbl (dec (length instructions))))

        (let [instruction (get instructions lbl)]
          (unless instruction
            (errorf "No instruction at position %d" lbl))

          (let [[inst reg a b] instruction]
            # Validate register exists
            (unless (has-key? registers reg)
              (errorf "Register '%s' not found at instruction %d" reg lbl))

            (case inst
              :inc (do (update registers reg inc)
                     (put env :pointer a))
              :deb (let [x (registers reg)]
                     # Validate register value is numeric
                     (unless (number? x)
                       (errorf "Register '%s' has non-numeric value at instruction %d" reg lbl))
                     (if (> x 0)
                       # its positive, so decrement and goto next=b
                       (do
                         (update registers reg dec)
                         (put env :pointer b))
                       # jump to a
                       (put env :pointer a)))
              :end (put env :halted true)
              (errorf "Unknown instruction type '%s' at position %d" inst lbl)))))))
    env))

(defn run
  "Run until halted or max-steps."
  [env &opt max-steps]
  (default max-steps (dyn *MAX-STEPS*))
  (var env env)
  (var steps 0)
  (while (and (not (env :halted))
              (< steps max-steps))
    (set env (step env))
    (++ steps))
  env)

(defn clone [env]
  @{:instructions (env :instructions)
    :registers (table/clone (env :registers))
    :pointer (env :pointer)
    :halted (env :halted)})
