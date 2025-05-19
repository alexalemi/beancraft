# Let's try to make this the main runner

(use ./parse)
(use ./env)
(use spork)

(defn main
  [& args]
  (let [fname (get args 1)
        f (file/open fname :r)
        content (file/read f :all)
        program (compile content (path/dirname fname))]
    (each arg (slice args 2)
      (let [[reg val] (string/split "=" arg)]
        (set ((program :registers) reg) (scan-number val))))

    (print)
    (print "Final compiled program:")
    (eachp [i inst] (get program :instructions)
      (prin i " ")
      (each k inst
        (prin k " "))
      (print))

    (def final (run (clone program)))

    (print)
    (print "Final Registers:")

    (eachp [reg val] (get final :registers)
      (prin reg)
      (prin ":")
      (print val))))
