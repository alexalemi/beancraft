# Let's try to make this the main runner

(use ./parse)
(use ./env)

(defn main
  [& args]
  (let [fname (get args 1)
        f (file/open fname :r)
        content (file/read f :all)
        program (compile content)]
    (each arg (slice args 2)
      (let [[reg val] (string/split ":" arg)]
        (set ((program :registers) reg) (scan-number val))))

    (eachp [reg val] (get (run (clone program)) :registers)
      (prin reg)
      (prin ":")
      (print val))))
