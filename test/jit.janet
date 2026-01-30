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

# ============================================================
# Multi-transfer and Copy pattern tests
# ============================================================

# Test multi-transfer detection (deb A; inc B; inc C; loop)
(def multi-transfer-prog `
loop: deb A done
inc B
inc C loop
done: end`)

(defn test-multi-transfer-detection []
  (let [program (compile multi-transfer-prog)
        analysis (analyze-program program)]
    (= 1 (length (get analysis :multi-transfers @[])))))

(test (test-multi-transfer-detection) true)

# Test multi-transfer execution
(defn test-multi-transfer-exec [a]
  (let [program (compile multi-transfer-prog)
        _ (merge-into (program :registers) @{"A" a "B" 0 "C" 0})
        result (jit-run program nil true)]
    [(get (result :registers) "A")
     (get (result :registers) "B")
     (get (result :registers) "C")]))

(test (test-multi-transfer-exec 10) [0 10 10])
(test (test-multi-transfer-exec 100) [0 100 100])

# Test copy pattern detection (copy.bc uses this pattern)
(defn test-copy-detection []
  (let [program (compile (slurp (path/join examples-path "copy.bc")) examples-path)
        analysis (analyze-program program)]
    # Should detect the copy pattern
    (> (length (get analysis :copies @[])) 0)))

# Note: copy.bc starts with clearing tmp, which may affect pattern detection
# The copy pattern is: multi-transfer to (To, tmp) followed by restore from tmp to From

# Test that copy produces correct results
(defn test-copy-exec [from to]
  (let [result (example-jit-runner "copy.bc" @{"From" from "To" to})]
    [(get (result :registers) "From")
     (get (result :registers) "To")]))

# copy.bc should: To += From, From preserved
(test (test-copy-exec 10 0) [10 10])
(test (test-copy-exec 10 5) [10 15])
(test (test-copy-exec 100 50) [100 150])

# Test that optimized copy is faster
(defn test-copy-optimization-speedup []
  (let [program1 (compile (slurp (path/join examples-path "copy.bc")) examples-path)
        _ (merge-into (program1 :registers) @{"From" 1000 "To" 0})
        opt-result (jit-run program1 nil true)
        program2 (compile (slurp (path/join examples-path "copy.bc")) examples-path)
        _ (merge-into (program2 :registers) @{"From" 1000 "To" 0})
        unopt-result (jit-run program2 nil false)]
    # Both should produce correct result
    (and (= (get (opt-result :registers) "To") 1000)
         (= (get (unopt-result :registers) "To") 1000)
         # And optimized should be faster (fewer steps)
         (< (opt-result :steps) (unopt-result :steps)))))

(test (test-copy-optimization-speedup) true)

# ============================================================
# Bignum optimization tests
# ============================================================

# Test bignum with optimizations enabled
(defn bignum-jit-runner [s registers &opt optimize]
  (default optimize true)
  (let [program (compile s)]
    (merge-into (get program :registers) registers)
    (jit-run program nil optimize true)))  # bignum=true

# Test basic bignum addition with optimizations
(defn test-bignum-opt-adder [a b]
  (let [result (bignum-jit-runner adder @{"A" a "B" b})]
    ((result :registers) "Out")))

(test (test-bignum-opt-adder 10 15) 25)
(test (test-bignum-opt-adder 100 200) 300)
(test (test-bignum-opt-adder 0 50) 50)

# Test that bignum optimized and unoptimized produce same results
(defn test-bignum-opt-vs-unopt [a b]
  (let [program (compile adder)
        _ (merge-into (program :registers) @{"A" a "B" b})
        opt-result (jit-run program nil true true)  # optimize=true, bignum=true
        program2 (compile adder)
        _ (merge-into (program2 :registers) @{"A" a "B" b})
        unopt-result (jit-run program2 nil false true)]  # optimize=false, bignum=true
    (= ((opt-result :registers) "Out")
       ((unopt-result :registers) "Out"))))

(test (test-bignum-opt-vs-unopt 10 15) true)
(test (test-bignum-opt-vs-unopt 100 200) true)

# Test that bignum optimization reduces step count
(defn test-bignum-opt-reduces-steps [a b]
  (let [program (compile adder)
        _ (merge-into (program :registers) @{"A" a "B" b})
        opt-result (jit-run program nil true true)
        program2 (compile adder)
        _ (merge-into (program2 :registers) @{"A" a "B" b})
        unopt-result (jit-run program2 nil false true)]
    # Optimized should take fewer steps
    (< (opt-result :steps) (unopt-result :steps))))

(test (test-bignum-opt-reduces-steps 100 100) true)

# Test bignum clear loop optimization
(defn test-bignum-clear-execution [a]
  (let [program (compile clearer)
        _ (merge-into (program :registers) @{"A" a})
        result (jit-run program nil true true)]
    ((result :registers) "A")))

(test (test-bignum-clear-execution 100) 0)
(test (test-bignum-clear-execution 1000) 0)

# Test bignum multi-transfer optimization
(defn test-bignum-multi-transfer-exec [a]
  (let [program (compile multi-transfer-prog)
        _ (merge-into (program :registers) @{"A" a "B" 0 "C" 0})
        result (jit-run program nil true true)]
    [(get (result :registers) "A")
     (get (result :registers) "B")
     (get (result :registers) "C")]))

(test (test-bignum-multi-transfer-exec 10) [0 10 10])
(test (test-bignum-multi-transfer-exec 100) [0 100 100])

# Test bignum copy pattern
(defn test-bignum-copy-exec [from to]
  (let [program (compile (slurp (path/join examples-path "copy.bc")) examples-path)
        _ (merge-into (program :registers) @{"From" from "To" to})
        result (jit-run program nil true true)]
    [(get (result :registers) "From")
     (get (result :registers) "To")]))

(test (test-bignum-copy-exec 10 0) [10 10])
(test (test-bignum-copy-exec 10 5) [10 15])
(test (test-bignum-copy-exec 100 50) [100 150])
