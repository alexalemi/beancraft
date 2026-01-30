# Tests for JIT compiler

(use ../beancraft/parse)
(use ../beancraft/jit)
(use ../beancraft/optimize)
(use judge)
(use spork)

(def examples-path (path/join (path/dirname (dyn :current-file)) ".." "examples"))

# Test basic addition program
(def adder `deb A other
inc Out prev
other: deb B halt
inc Out prev`)

(defn jit-runner [s registers &opt optimize]
  (default optimize true)
  (let [program (compile s)]
    (merge-into (get program :registers) registers)
    (jit-run program nil optimize)))

(defn test-jit-adder [a b]
  (let [result (jit-runner adder @{"A" a "B" b})]
    ((result :registers) "Out")))

(test (test-jit-adder 10 15) 25)
(test (test-jit-adder 0 5) 5)
(test (test-jit-adder 10 10) 20)
(test (test-jit-adder 100 200) 300)

# Test that JIT produces same results as interpreter
(defn example-jit-runner [flname registers]
  (let [program (compile (slurp (path/join examples-path flname)) examples-path)]
    (merge-into (get program :registers) registers)
    (jit-run program)))

(defn test-examples-jit-adder [a b]
  ((get (example-jit-runner "add.bc" @{"A" a "B" b}) :registers) "Out"))

(test (test-examples-jit-adder 10 15) 25)
(test (test-examples-jit-adder 0 5) 5)
(test (test-examples-jit-adder 10 10) 20)

# Test addWithFunc through JIT
(defn test-jit-addWithFunc [a b]
  ((get (example-jit-runner "addWithFunc.bc" @{"A" a "B" b}) :registers) "Out"))

(test (test-jit-addWithFunc 10 15) 25)
(test (test-jit-addWithFunc 0 5) 5)
(test (test-jit-addWithFunc 10 10) 20)

# Test that halted flag is set correctly
(test (get (jit-runner adder @{"A" 5 "B" 3}) :halted) true)

# Test step counting
(test (> (get (jit-runner adder @{"A" 100 "B" 100}) :steps) 0) true)

# Test generated code structure
(defn test-code-generation []
  (let [program (compile adder)
        code-str (show-generated-code program)]
    # Should contain function definition
    (truthy? (string/find "fn" code-str))))

(test (test-code-generation) true)

# Test copy program
(defn jit-copy-runner [a b]
  (let [result (example-jit-runner "copy.bc" @{"From" a "To" b})
        registers (result :registers)]
    {"From" (get registers "From") "To" (get registers "To")}))

(test (jit-copy-runner 10 5) {"From" 10 "To" 15})
(test (jit-copy-runner 10 0) {"From" 10 "To" 10})
(test (jit-copy-runner 13 0) {"From" 13 "To" 13})

# ============================================================
# Optimizer tests
# ============================================================

# Test that optimizer detects transfer loops
(defn test-optimizer-detection []
  (let [program (compile adder)
        analysis (analyze-program program)]
    # Should detect 2 transfer loops (A->Out and B->Out)
    (= 2 (length (analysis :loops)))))

(test (test-optimizer-detection) true)

# Test that optimized and unoptimized produce same results
(defn test-opt-vs-unopt [a b]
  (let [program (compile adder)
        _ (merge-into (program :registers) @{"A" a "B" b})
        opt-result (jit-run program nil true)
        program2 (compile adder)
        _ (merge-into (program2 :registers) @{"A" a "B" b})
        unopt-result (jit-run program2 nil false)]
    (= ((opt-result :registers) "Out")
       ((unopt-result :registers) "Out"))))

(test (test-opt-vs-unopt 10 15) true)
(test (test-opt-vs-unopt 100 200) true)
(test (test-opt-vs-unopt 0 50) true)

# Test that optimization reduces step count
(defn test-opt-reduces-steps [a b]
  (let [program (compile adder)
        _ (merge-into (program :registers) @{"A" a "B" b})
        opt-result (jit-run program nil true)
        program2 (compile adder)
        _ (merge-into (program2 :registers) @{"A" a "B" b})
        unopt-result (jit-run program2 nil false)]
    # Optimized should take fewer steps
    (< (opt-result :steps) (unopt-result :steps))))

(test (test-opt-reduces-steps 100 100) true)
(test (test-opt-reduces-steps 1000 1000) true)

# Test clear loop detection
(def clearer `loop: deb A done self
done: end`)

(defn test-clear-loop []
  (let [program (compile clearer)
        analysis (analyze-program program)]
    # Should detect 1 clear loop
    (and (= 1 (length (analysis :loops)))
         (= :clear (get-in analysis [:loops 0 :type])))))

(test (test-clear-loop) true)

# Test clear loop execution
(defn test-clear-execution [a]
  (let [program (compile clearer)
        _ (merge-into (program :registers) @{"A" a})
        result (jit-run program nil true)]
    ((result :registers) "A")))

(test (test-clear-execution 100) 0)
(test (test-clear-execution 0) 0)
(test (test-clear-execution 10000) 0)

# Test add pattern detection
(defn test-add-pattern-detection []
  (let [program (compile adder)
        analysis (analyze-program program)]
    # Should detect the add pattern (two transfers to Out)
    (= 1 (length (analysis :adds)))))

(test (test-add-pattern-detection) true)

# Test multiplication with large values (would timeout without optimization)
(defn test-mul-large [a b]
  (let [result (example-jit-runner "mul.bc" @{"A" a "B" b})]
    ((result :registers) "Out")))

# These would be very slow without optimization
(test (test-mul-large 100 100) 10000)
(test (test-mul-large 50 200) 10000)
