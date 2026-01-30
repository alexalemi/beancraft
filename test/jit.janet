# Tests for JIT compiler

(use ../beancraft/parse)
(use ../beancraft/jit)
(use judge)
(use spork)

(def examples-path (path/join (path/dirname (dyn :current-file)) ".." "examples"))

# Test basic addition program
(def adder `deb A other
inc Out prev
other: deb B halt
inc Out prev`)

(defn jit-runner [s registers]
  (let [program (compile s)]
    (merge-into (get program :registers) registers)
    (jit-run program)))

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
