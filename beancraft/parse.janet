# Parses the Bins and Beans language.

# Tries to parse the file in a set of tuples of size 4.
# [ label :end ]
# [ label :inc register next ]
# [ label :dec register jump next ]
# [ label :use flname scope  reg alias  reg2 alias2 ]

(def grammar
  ~{:commands (+ "load" "func")
    :keyword (+ "self" "next" "prev" "init" "done" "halt" "end")
    :name (some :w)
    :space (any (set " \t\r\0\f\v")) 
    :number (some :d)
    :move (* (+ "+" "-") (some :d))
    :register (+ (number :number) (<- :name))
    :jump (+ (/ (<- :keyword) ,keyword) (number :move) (<- :name))
    :none (* 0 (constant nil))
    :inc (* (+ "inc" "+") (constant :inc) :space :register :space (+ :jump :none) :space)
    :deb (* (+ "deb" "-") (constant :deb) :space :register :space :jump :space (+ :jump :none) :space)
    :end (* (+ "end" ".") (constant :end) :space)
    :use (* (+ "use" "%") (constant :use) :space "\"" (<- :filename) "\"" (+ (* ":" (<- :name)) :none) (any (* :space :register ":" :register)) :space)
    :label (* :space (<- :name) ":" :space)
    :newline (+ -1 "\n")
    :filename (some (if-not (set " \t\r\n\0\f\v\"\'") 1))
    :line (group (* (+ :label :none) (+ :inc :deb :end :use) :newline))
    :comment (* "#" (some (if-not "\n" 1)) :newline)
    :main (some (+ :comment :newline :line))})


(defn parse
  "Top level read of program."
  [s]
  (peg/match grammar s))

(defn read-labels-and-registers
  "Pull out all of the labels and registers and flatten the instructions."
  [instructions]
  (let [labels @{}
        registers @{}
        program @[]]
    (eachp [i [lbl inst & more]] instructions
      (when lbl (set (labels lbl) (length program)))
      (when (not= inst :use)
        (let [[reg & rest] more]
          (when reg (set (registers reg) 0))))
      (array/push program [inst ;more]))
    {:labels labels
     :registers registers
     :instructions program
     :pointer 0
     :halted false}))

(defn add-halt
  "Add a final halt to the end of the program"
  [program]
  (array/push (get program :instructions) [:end])
  program)

(defn replace-jumps
  "Replace the jumps with absolute positions for moves, addresses for labels and relatant positions for labels."
  [program]
  (let [{:instructions instructions :labels labels} program
        extra-labels (merge labels {:done :done :halt :halt})] 
    (eachp [i [inst reg nxt jmp]] instructions
      (let [extra-labels (merge labels {:done (length instructions) :halt (dec (length instructions)) :end (dec (length instructions)) :next (inc i) nil (inc i) :prev (dec i) :self i :init 0})]
        (case inst
          :inc (if nxt
                 (put instructions i [inst reg (if (number? nxt) (+ i nxt) (get extra-labels nxt))])
                 (put instructions i [inst reg (inc i)]))
          :deb (if jmp
                 (put instructions i [inst reg (if (number? nxt) (+ i nxt) (get extra-labels nxt)) (if (number? jmp) (+ i jmp) (get extra-labels jmp))])
                 (if nxt
                   (put instructions i [inst reg (inc i) (if (number? nxt) (+ i nxt) (get extra-labels nxt))])
                   (put instructions i [inst reg (inc i) (get extra-labels :halt)]))))))
    program))

(defn compile
  "Compiler passes
  
  Takes a sugar version and compiles to a simple program."
  [s]
  (-> s
      parse
      # first read out all of the labels and registers
      read-labels-and-registers
      # ensure each block ends with a halt
      add-halt 
      # replace the relative addresses, keywords and labels
      replace-jumps

      # final program halt
      add-halt)) 
    
    

(comment
  (compile `# This is a comment

init: - A end
+ B init`))

(comment
  (compile `use "other.bb":foo a:b c:d e:f 0:1 3:foo
deb 1 +1 +2
inc 0 -1
deb 2 +1 done
inc 0 -2
end`))


(comment
  (compile `# A better syntax version of the add machine
# this moves the contents of A and B into the Out register.

init: - A copyB
+ Out prev

copyB: - B done
+ Out prev`))
