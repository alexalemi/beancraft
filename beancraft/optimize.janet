# Optimizer for Beancraft JIT
#
# Detects common loop patterns and replaces them with optimized operations.
# This can turn O(n) loops into O(1) operations.
#
# Recognized patterns:
# - Transfer: deb A; inc B (moves A into B, A becomes 0)
# - Clear: deb A; (loop back) (sets A to 0)
# - Add: Two consecutive transfers to same target

(defn- find-loop-at
  "Check if instruction at `idx` starts a simple loop.
   Returns loop info or nil.

   A simple loop is: deb REG exit; inc REG2 loop-start
   Where the inc jumps back to the deb."
  [instructions idx]
  (when (< idx (length instructions))
    (let [[op reg exit-target next-target] (get instructions idx)]
      (when (= op :deb)
        # Check if next-target points to an inc that loops back
        (when (and next-target
                   (< next-target (length instructions))
                   (not= next-target idx))  # not self-loop on deb
          (let [[next-op next-reg next-next] (get instructions next-target)]
            (when (and (= next-op :inc)
                       (= next-next idx))  # inc loops back to our deb
              {:type :transfer
               :start idx
               :src-reg reg
               :dst-reg next-reg
               :exit exit-target
               :loop-body next-target})))))))

(defn- find-clear-loop-at
  "Check if instruction at `idx` is a clear loop (deb that loops to itself).
   Pattern: deb A exit self  OR  deb A exit; ... ; jmp start"
  [instructions idx]
  (when (< idx (length instructions))
    (let [[op reg exit-target next-target] (get instructions idx)]
      (when (= op :deb)
        # Direct self-loop: deb A exit self
        (when (= next-target idx)
          {:type :clear
           :start idx
           :reg reg
           :exit exit-target})))))

(defn- find-all-loops
  "Find all simple loops in the program."
  [instructions]
  (def loops @[])
  (for i 0 (length instructions)
    (when-let [loop-info (find-loop-at instructions i)]
      (array/push loops loop-info))
    (when-let [clear-info (find-clear-loop-at instructions i)]
      (array/push loops clear-info)))
  loops)

(defn- find-add-patterns
  "Find consecutive transfer loops that form an add pattern.
   Pattern: transfer A->Out, then transfer B->Out
   This adds A+B into Out."
  [instructions loops]
  (def adds @[])
  (each loop loops
    (when (= (loop :type) :transfer)
      # Check if exit leads to another transfer to same destination
      (let [exit-idx (loop :exit)]
        (each other-loop loops
          (when (and (= (other-loop :type) :transfer)
                     (= (other-loop :start) exit-idx)
                     (= (other-loop :dst-reg) (loop :dst-reg))
                     (not= (other-loop :src-reg) (loop :src-reg)))
            (array/push adds {:type :add
                              :first-loop loop
                              :second-loop other-loop
                              :dst-reg (loop :dst-reg)
                              :src-regs [(loop :src-reg) (other-loop :src-reg)]
                              :exit (other-loop :exit)}))))))
  adds)

(defn analyze-program
  "Analyze a program and return optimization info."
  [program]
  (let [instructions (program :instructions)
        loops (find-all-loops instructions)
        adds (find-add-patterns instructions loops)]
    {:loops loops
     :adds adds
     :instruction-count (length instructions)}))

(defn- safe-symbol
  "Convert a register name to a safe Janet symbol."
  [name]
  (if (keyword? name)
    (symbol (string "_" name))
    (-> (string name)
        (string/replace-all "/" "_")
        (string/replace-all "-" "_")
        symbol)))

(defn- generate-optimized-transfer
  "Generate optimized code for a transfer loop.
   Instead of looping, do: dst += src; src = 0"
  [loop-info sym-map]
  (let [src-sym (sym-map (loop-info :src-reg))
        dst-sym (sym-map (loop-info :dst-reg))
        exit (loop-info :exit)]
    # Return code that transfers and jumps to exit
    ~(do
       (+= ,dst-sym ,src-sym)
       (set ,src-sym 0)
       (set _pc ,exit))))

(defn- generate-optimized-clear
  "Generate optimized code for a clear loop.
   Instead of looping, do: reg = 0"
  [loop-info sym-map]
  (let [reg-sym (sym-map (loop-info :reg))
        exit (loop-info :exit)]
    ~(do
       (set ,reg-sym 0)
       (set _pc ,exit))))

(defn- generate-optimized-add
  "Generate optimized code for an add pattern.
   Instead of two transfer loops, do: dst += src1 + src2; src1 = 0; src2 = 0"
  [add-info sym-map]
  (let [src1-sym (sym-map (get-in add-info [:src-regs 0]))
        src2-sym (sym-map (get-in add-info [:src-regs 1]))
        dst-sym (sym-map (add-info :dst-reg))
        exit (add-info :exit)]
    ~(do
       (+= ,dst-sym ,src1-sym)
       (+= ,dst-sym ,src2-sym)
       (set ,src1-sym 0)
       (set ,src2-sym 0)
       (set _pc ,exit))))

(defn generate-optimized-instruction
  "Generate optimized code for an instruction, using loop optimizations if available.
   Returns [code, skip-count] where skip-count is how many instructions to skip."
  [idx instructions analysis sym-map]
  (let [loops (analysis :loops)
        adds (analysis :adds)]

    # Check if this starts an add pattern (takes priority)
    (var found-add nil)
    (each add adds
      (when (= (get-in add [:first-loop :start]) idx)
        (set found-add add)
        (break)))

    (if found-add
      [(generate-optimized-add found-add sym-map)
       # Skip both loop bodies
       nil]

      # Check if this starts a loop
      (do
        (var found-loop nil)
        (each loop loops
          (when (= (loop :start) idx)
            (set found-loop loop)
            (break)))

        (if found-loop
          (case (found-loop :type)
            :transfer [(generate-optimized-transfer found-loop sym-map) nil]
            :clear [(generate-optimized-clear found-loop sym-map) nil]
            [nil nil])
          [nil nil])))))

(defn get-optimized-starts
  "Get a set of instruction indices that start optimized patterns."
  [analysis]
  (def starts @{})
  (each loop (analysis :loops)
    (put starts (loop :start) loop))
  (each add (analysis :adds)
    # Add pattern starts at first loop's start
    (put starts (get-in add [:first-loop :start]) add))
  starts)

(defn get-skipped-instructions
  "Get a set of instruction indices that are part of optimized loops
   and should be skipped (but might still be jumped to from outside)."
  [analysis]
  (def skipped @{})
  (each loop (analysis :loops)
    (when (= (loop :type) :transfer)
      # The inc instruction in a transfer loop is handled by the optimization
      (put skipped (loop :loop-body) true)))
  # For add patterns, mark inner instructions as potentially skipped
  (each add (analysis :adds)
    (put skipped (get-in add [:first-loop :loop-body]) true)
    (put skipped (get-in add [:second-loop :start]) true)
    (put skipped (get-in add [:second-loop :loop-body]) true))
  skipped)
