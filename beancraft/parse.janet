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
    :line (group (* (+ :label (line)) (+ :inc :deb :end :use) :newline))
    :comment (* "#" (some (if-not "\n" 1)) :newline)
    :main (some (+ :comment :newline :line))})

(defn parse
  "Top level parse function"
  [s]
  (peg/match grammar s))

(comment
  (parse `# This is a comment

init: - A end
+ B init`))

(comment
  (parse `use "other.bb":foo a:b c:d e:f 0:1 3:foo
deb 1 +1 +2
inc 0 -1
deb 2 +1 done
inc 0 -2
end`))


(comment
  (parse `# A better syntax version of the add machine
# this moves the contents of A and B into the Out register.

init: - A copyB
+ Out prev

copyB: - B done
+ Out prev`))
