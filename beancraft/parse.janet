# Parses the Bins and Beans language.

(def grammar
  ~{:commands (+ "load" "func")
    :keyword (+ "self" "next" "prev" "init" "done" "halt")
    :name (some :w)
    :space (any (set " \t\r\0\f\v")) 
    :number (some :d)
    :register (+ (number :number) (<- :name))
    :jump (+ (/ (<- :keyword) ,keyword) (number :number) (<- :name))
    :none (* 0 (constant nil))
    :inc (* (+ "inc" "+") (constant :inc) :space :register :space (+ :jump :none) :space)
    :deb (* (+ "deb" "-") (constant :deb) :space :register :space :jump :space (+ :jump :none) :space)
    :end (* (+ "end" ".") (constant :end) :space)
    :label (* :space (<- :name) ":" :space)
    :newline (+ -1 "\n")
    :filename (some (if-not (set " \t\r\n\0\f\v\"\'") 1))
    :line (group (* (+ :label (line)) (+ :inc :deb :end) :newline))
    :load (group (* (line) "load" (constant :load) :space "\"" (<- :filename) "\"" :space :newline))
    :comment (* "#" (some (if-not "\n" 1)) :newline)
    :main (some (+ :load :comment :line))})

# TODO: need to add support for the funcs. 

(comment
  (peg/match grammar `# This is a comment
init: - A end
+ B init`))


(comment
  (peg/match grammar
    `load "other.bb"
deb 1 2 3
inc 0 1
deb 2 4 5
inc 0 3
end`))

