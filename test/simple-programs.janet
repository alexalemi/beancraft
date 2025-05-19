
(use ../beancraft/parse)
(use judge)
(use ../beancraft/env)
(use spork)

(def examples-path (path/join (path/dirname (dyn :current-file)) ".." "examples"))

(defn runner [s registers]
  (let [program (compile s)]
    (merge-into (get program :registers) registers)
    (get (run program) :registers)))


(def adder `deb A other
inc Out prev
other: deb B halt
inc Out prev`)

(defn test-adder [a b]
  ((runner adder @{"A" a "B" b}) "Out"))
  
(test (test-adder 10 15) 25)
  
(test (test-adder 0 5) 5)

(test (test-adder 10 10) 20)


(defn example-runner [flname registers]
  (let [program (compile (slurp (path/join examples-path flname)) examples-path)]
    (merge-into (get program :registers) registers)
    (get (run program) :registers)))

(defn test-examples-adder [a b]
  ((example-runner "add.bc" @{"A" a "B" b}) "Out"))

(test (test-examples-adder 10 15) 25)
  
(test (test-examples-adder 0 5) 5)

(test (test-examples-adder 10 10) 20)

(defn test-addWithFunc [a b]
  ((example-runner "addWithFunc.bc" @{"A" a "B" b}) "Out"))

(test (test-addWithFunc 10 15) 25)
  
(test (test-addWithFunc 0 5) 5)

(test (test-addWithFunc 10 10) 20)

(test ((example-runner "addWithFunc.bc" @{"A" 3 "B" 4 "Out" 5}) "Out") 7)
