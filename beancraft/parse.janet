# Parses the Bins and Beans language.

# Tries to parse the file in a set of tuples of size 4.
# [ label :end ]
# [ label :inc register next ]
# [ label :dec register jump next ]

# label: use "flname":scope reg=alias reg2=alias2 label~newlabel label2~newlabel2
# [ label :use flname scope  reg alias  reg2 alias2 ]

(use spork)
(use judge)

(def BEANCRAFT-ROOT (os/getenv "BEANCRAFTROOT" "~/.beancraft/"))

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
    :use (* (+ "use" "%") (constant :use) :space "\"" (<- :filename) "\"" (+ (* ":" (<- :name)) :none) (group (any (* :space :register "=" :register))) (group (any (* :space :labelName "~" :labelName))) :space)
    :labelName (<- :name)
    :label (* :space :labelName ":" :space)
    :newline (+ -1 "\n")
    :filename (some (if-not (set " \t\r\n\0\f\v\"\'") 1))
    :line (group (* (+ :label :none) (+ :inc :deb :end :use) :newline))
    :comment (* "#" (some (if-not "\n" 1)) :newline)
    :main (some (+ :comment :newline :line))})


(defn parse
  "Top level read of program."
  [s]
  (peg/match grammar s))


(test parse @parse)

(defn read-labels-and-registers
  "Pull out all of the labels and registers and flatten the instructions."
  [instructions &opt start]
  (default start 0)
  (let [labels @{}
        registers @{}
        program @[]]
    (eachp [i [lbl inst & more]] instructions
      (when lbl (set (labels lbl) (+ start (length program))))
      (when (not= inst :use)
        (let [[reg & rest] more]
          (when reg (set (registers reg) 0))))
      (array/push program [inst ;more]))
    @{:labels labels
      :registers registers
      :instructions program
      :pointer 0
      :halted false}))

(defn add-halt
  "Add a final halt to the end of the program"
  [program]
  (array/push (get program :instructions) [:end])
  program)

(defn add-nil
  "Add a final halt to the end of the program"
  [program]
  (set ((get program :registers) :nil) 0)
  program)

(defn replace-jumps
  "Replace the jumps with absolute positions for moves, addresses for labels and relatant positions for labels."
  [program &opt init done halt]
  (let [{:instructions instructions :labels labels} program]
    (default init 0)
    (default done (length instructions))
    (default halt (dec (length instructions)))
    (eachp [i [inst reg a b]] instructions
      (let [me (+ i init)
            extra-labels (merge {:done done :halt halt :end halt :init init
                                 :next (inc me) nil (inc me) :prev (dec me) :self me} labels)]
        (case inst
          :inc (if a
                 # if we have an explicit next, set it
                 (put instructions i [inst reg (if (number? a) (+ me a) (get extra-labels a))])
                 # otherwise goto next by default
                 (put instructions i [inst reg (inc me)]))
          :deb (if b
                 # deb with two instructions, treat them as jmp then nxt
                 (put instructions i [inst reg (if (number? a) (+ me a) (get extra-labels a)) (if (number? b) (+ me b) (get extra-labels b))])
                 # otherwise only one argument, treat it as the jmp
                 (put instructions i [inst reg (if (number? a) (+ me a) (get extra-labels a)) (inc me)])))))
    program))

(defn add-done
  "Add a final jump to the end of the program for done"
  [program loc]
  (array/push (get program :instructions) [:deb :nil loc loc])
  program)

(defn replace-labels
  "Inside a use, reroute the given labels to original."
  [program labels original-labels]
  (let [{:labels program-labels} program]
    (eachk lbl program-labels
      (when (get labels lbl)
        (set (program-labels lbl) (get original-labels (get labels lbl))))))
  program)

(defn replace-registers
  "Before we do the rest of the parsing, go and replace the registers with either their target or a scoped version."
  [instructions scope registers]
  (eachp [i [lbl inst reg & more]] instructions
    (case inst
      :inc (put instructions i [lbl inst (if (get registers reg) (get registers reg) (string scope "/" reg)) ;more])
      :deb (put instructions i [lbl inst (if (get registers reg) (get registers reg) (string scope "/" reg)) ;more])))
  instructions)

(defn separate-regs-and-values
  "Separate register aliases from initial values"
  [regs]
  (let [aliases @[]
        values @[]]
    (each [reg-name value] (partition 2 regs)
      (if (number? value)
        (array/push values reg-name value)
        (array/push aliases reg-name value)))
    [aliases values]))

(defn set-initial-values
  "Set initial values for registers when the mapping value is a number"
  [program values scope]
  (let [{:registers registers} program]
    (each [reg-name value] (partition 2 values)
      (let [scoped-reg (string scope "/" reg-name)]
        (set (registers scoped-reg) value))))
  program)

(defn compile-use
  "Compiler passes for a use command."
  [flname root loc start scope regs labels original-labels replace-use]
  (default scope (string flname "-" loc))
  (let [path (path/join root (string flname ".bc"))
        [aliases values] (separate-regs-and-values regs)]
    (-> (slurp path)
        parse
        (replace-registers scope (table ;aliases))
        (read-labels-and-registers start)
        (set-initial-values values scope)
        (replace-labels (table ;labels) original-labels)
        (replace-jumps start (inc loc))
        (add-done loc)
        (replace-use root start))))

(defn replace-use
  "Replace all of the use commands"
  [program path &opt offset]
  (default offset 0)
  (let [{:instructions instructions :labels labels :registers registers} program
        extra-labels (merge labels {:done :done :halt :halt})]
    (eachp [i [inst flname scope regs lbs]] instructions
      (when (= inst :use)
        (let [start (+ (length instructions) offset)
              {:instructions use-instructions :labels use-labels :registers use-registers} (compile-use flname path (+ i offset) start scope regs lbs labels replace-use)]
          (put instructions i [:deb :nil start start])
          (array/join instructions use-instructions)
          (merge-into labels use-labels)
          (merge-into registers use-registers))))
    program))

(defn compile
  "Compiler passes
  
  Takes a sugar version and compiles to a simple program."
  [s &opt path]
  (default path BEANCRAFT-ROOT)
  (-> s
      parse
      # first read out all of the labels and registers
      read-labels-and-registers
      # ensure each block ends with a halt
      add-halt
      # replace the relative addresses, keywords and labels
      replace-jumps
      # replace the use commands
      (replace-use path)
      # final program halt
      add-halt
      add-nil))


(test
  (parse `# This is a comment
use "add" A=3
init: - A end
+ B init`)
  @[@[nil :use "add" nil @["A" 3] @[]]
    @["init" :deb "A" :end nil]
    @[nil :inc "B" :init]])

(test
  (compile `# This is a comment
use "add" A=3
init: - A end
+ B init` "/home/alemi/projects/beancraft/examples/")
  @{:halted false
    :instructions @[[:deb :nil 4 4]
                    [:deb "A" 3 2]
                    [:inc "B" 0]
                    [:end]
                    [:deb "add-0/A" 6 5]
                    [:inc "add-0/Out" 4]
                    [:deb "add-0/B" 1 7]
                    [:inc "add-0/Out" 6]
                    [:deb :nil 0 0]
                    [:end]]
    :labels @{"copyB" 6 "init" 4}
    :pointer 0
    :registers @{"A" 0
                 "B" 0
                 "add-0/A" 3
                 "add-0/B" 0
                 "add-0/Out" 0
                 :nil 0}})


(test
  (compile `# A better syntax version of the add machine
# this moves the contents of A and B into the Out register.

init: - A copyB
+ Out prev

copyB: - B done
+ Out prev`)
  @{:halted false
    :instructions @[[:deb "A" 2 1]
                    [:inc "Out" 0]
                    [:deb "B" 5 3]
                    [:inc "Out" 2]
                    [:end]
                    [:end]]
    :labels @{"copyB" 2 "init" 0}
    :pointer 0
    :registers @{"A" 0 "B" 0 "Out" 0 :nil 0}})
