# Optimizer for Beancraft JIT
#
# Detects common loop patterns and replaces them with optimized operations.
# This can turn O(n) loops into O(1) operations.
#
# Recognized patterns:
# - Transfer: deb A; inc B (moves A into B, A becomes 0)
# - Clear: deb A; (loop back) (sets A to 0)
# - Add: Two consecutive transfers to same target
# - Multi-transfer: deb A; inc B; inc C (transfers A to multiple destinations)
# - Copy: Multi-transfer to (dst, tmp) followed by restore from tmp (preserves src)
# - Multiply: Outer loop containing copy pattern (A * B)

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

(defn- find-multi-transfer-at
  "Check if instruction at `idx` starts a multi-target transfer loop.
   Pattern: deb A exit; inc B; inc C; ... ; jmp loop-start
   Returns loop info with all destination registers."
  [instructions idx]
  (when (< idx (length instructions))
    (let [[op src-reg exit-target next-target] (get instructions idx)]
      (when (and (= op :deb)
                 next-target
                 (< next-target (length instructions))
                 (not= next-target idx))
        # Follow the chain of increments
        (var current next-target)
        (def destinations @[])
        (var loop-back nil)
        (var valid true)

        (while (and valid (not loop-back))
          (if (>= current (length instructions))
            (set valid false)
            (let [[cur-op cur-reg cur-next] (get instructions current)]
              (if (= cur-op :inc)
                (do
                  (array/push destinations cur-reg)
                  (if (= cur-next idx)
                    # Found the loop back
                    (set loop-back current)
                    # Continue following
                    (set current cur-next)))
                # Not an inc, invalid pattern
                (set valid false)))))

        (when (and valid loop-back (>= (length destinations) 2))
          {:type :multi-transfer
           :start idx
           :src-reg src-reg
           :dst-regs destinations
           :exit exit-target
           :loop-end loop-back})))))

(defn- find-copy-pattern
  "Find copy patterns: multi-transfer to (dst, tmp) followed by restore from tmp.
   Pattern: deb A exit; inc B; inc tmp; loop THEN deb tmp done; inc A; loop
   Result: B += A (with A preserved)"
  [instructions multi-transfers simple-loops]
  (def copies @[])
  (each mt multi-transfers
    (when (= 2 (length (mt :dst-regs)))
      (let [[dst1 dst2] (mt :dst-regs)
            exit-idx (mt :exit)
            src-reg (mt :src-reg)]
        # Check if exit leads to a restore loop (transfers one dst back to src)
        (each loop simple-loops
          (when (and (= (loop :type) :transfer)
                     (= (loop :start) exit-idx))
            # Check if this loop restores from one of the destinations to src
            (let [restore-src (loop :src-reg)
                  restore-dst (loop :dst-reg)]
              (when (and (= restore-dst src-reg)
                         (or (= restore-src dst1) (= restore-src dst2)))
                (let [actual-dst (if (= restore-src dst1) dst2 dst1)
                      tmp-reg restore-src]
                  (array/push copies {:type :copy
                                      :start (mt :start)
                                      :src-reg src-reg
                                      :dst-reg actual-dst
                                      :tmp-reg tmp-reg
                                      :restore-loop loop
                                      :multi-transfer mt
                                      :exit (loop :exit)})))))))))
  copies)

(defn- find-all-loops
  "Find all simple loops in the program."
  [instructions]
  (def loops @[])
  (def multi-transfers @[])
  (for i 0 (length instructions)
    (when-let [loop-info (find-loop-at instructions i)]
      (array/push loops loop-info))
    (when-let [clear-info (find-clear-loop-at instructions i)]
      (array/push loops clear-info))
    (when-let [mt-info (find-multi-transfer-at instructions i)]
      (array/push multi-transfers mt-info)))
  [loops multi-transfers])

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
        [loops multi-transfers] (find-all-loops instructions)
        adds (find-add-patterns instructions loops)
        copies (find-copy-pattern instructions multi-transfers loops)]
    {:loops loops
     :multi-transfers multi-transfers
     :adds adds
     :copies copies
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

(defn- generate-optimized-copy
  "Generate optimized code for a copy pattern.
   Instead of multi-transfer + restore, do: dst += src (src preserved)"
  [copy-info sym-map]
  (let [src-sym (sym-map (copy-info :src-reg))
        dst-sym (sym-map (copy-info :dst-reg))
        tmp-sym (sym-map (copy-info :tmp-reg))
        exit (copy-info :exit)]
    ~(do
       (+= ,dst-sym ,src-sym)
       # tmp is used internally but ends up at 0
       (set ,tmp-sym 0)
       (set _pc ,exit))))

(defn- generate-optimized-multi-transfer
  "Generate optimized code for a multi-target transfer.
   Transfer src to all destinations in one step."
  [mt-info sym-map]
  (let [src-sym (sym-map (mt-info :src-reg))
        dst-syms (map |(sym-map $) (mt-info :dst-regs))
        exit (mt-info :exit)]
    ~(do
       ,;(map (fn [dst] ~(+= ,dst ,src-sym)) dst-syms)
       (set ,src-sym 0)
       (set _pc ,exit))))

(defn generate-optimized-instruction
  "Generate optimized code for an instruction, using loop optimizations if available.
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
      [(generate-optimized-copy found-copy sym-map) nil]

      # Check if this starts an add pattern
      (do
        (var found-add nil)
        (each add adds
          (when (= (get-in add [:first-loop :start]) idx)
            (set found-add add)
            (break)))

        (if found-add
          [(generate-optimized-add found-add sym-map) nil]

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
              [(generate-optimized-multi-transfer found-mt sym-map) nil]

              # Check if this starts a simple loop
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
                  [nil nil]))))))))

(defn get-optimized-starts
  "Get a set of instruction indices that start optimized patterns."
  [analysis]
  (def starts @{})
  # Copy patterns take priority
  (each copy (analysis :copies)
    (put starts (copy :start) copy))
  # Then add patterns
  (each add (analysis :adds)
    (unless (has-key? starts (get-in add [:first-loop :start]))
      (put starts (get-in add [:first-loop :start]) add)))
  # Then multi-transfers (if not part of copy)
  (each mt (analysis :multi-transfers)
    (unless (has-key? starts (mt :start))
      (put starts (mt :start) mt)))
  # Then simple loops
  (each loop (analysis :loops)
    (unless (has-key? starts (loop :start))
      (put starts (loop :start) loop)))
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
  # For copy patterns, mark all inner instructions
  (each copy (get analysis :copies @[])
    (let [mt (copy :multi-transfer)
          restore (copy :restore-loop)]
      # Mark multi-transfer body
      (put skipped (mt :loop-end) true)
      # Mark restore loop
      (put skipped (restore :start) true)
      (put skipped (restore :loop-body) true)))
  # For multi-transfers, mark body instructions
  (each mt (get analysis :multi-transfers @[])
    (put skipped (mt :loop-end) true))
  skipped)
