# The main environment for bins and beans.

# An environment is a struct with some registers and some symbols defined.

# instructions are :inc :deb and :end
# :inc register next
# :deb register jump next
# :end 

(defdyn *MAX-STEPS*)
(setdyn *MAX-STEPS* 10_000)

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
      (let [[inst reg a b] (get instructions lbl)]
        (case inst
          :inc (do (update registers reg inc)
                 (put env :pointer a))
          :deb (let [x (registers reg)]
                 (if (> x 0)
                   # its positive, so decrement and goto next=b
                   (do
                     (update registers reg dec)
                     (put env :pointer b))
                   # jump to a
                   (put env :pointer a)))
          :end (put env :halted true))))
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

(comment
  (def adder @{:instructions [[:deb :A 1 2] [:inc :B 0] [:end]]
               :registers @{:A 5 :B 3}
               :pointer 0
               :halted false})
  [adder (run (clone adder))])
