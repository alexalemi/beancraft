# Let's try to make this the main runner

(use ./parse)

(defn main
  [& args]
  (let [fname (get args 1)
        f (file/open fname :r)
        content (file/read f :all)]
    (pp (parse content))))
