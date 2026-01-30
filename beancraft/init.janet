# Beancraft - A simple register machine
# Main CLI entry point

(use ./parse)
(use ./env)
(use ./jit)
(use spork)
(import spork/argparse :prefix "")

(def argparse-params
  ["A simple register machine compiler and executor."
   "file" {:kind :option
           :short "f"
           :help "Program file to run (.bc)"}
   "list-registers" {:kind :flag
                     :short "l"
                     :help "List all registers after compilation and exit"}
   "dry-run" {:kind :flag
              :short "n"
              :help "Compile only, show program without running"}
   "max-steps" {:kind :option
                :short "s"
                :help "Maximum execution steps (default: 10000)"}
   "quiet" {:kind :flag
            :short "q"
            :help "Quiet mode - don't print compiled program"}
   "verbose" {:kind :flag
              :short "v"
              :help "Verbose output - show step count after execution"}
   "jit" {:kind :flag
          :short "j"
          :help "Use JIT compilation for faster execution"}
   "show-jit" {:kind :flag
               :help "Show generated JIT code and exit"}
   :default {:kind :accumulate
             :help "Program file followed by REG=VALUE assignments"}])

(defn parse-register-args
  "Parse REG=VALUE arguments from command line.
   Returns a table of {register-name value}."
  [args]
  (def result @{})
  (each arg args
    (when (string/find "=" arg)
      (let [[reg val] (string/split "=" arg 0 2)]
        (when (and reg val (not (empty? reg)))
          (put result reg (scan-number val))))))
  result)

(defn get-program-file
  "Get the program file from args - either from --file or first positional arg."
  [parsed-args]
  (or (parsed-args "file")
      (when-let [defaults (parsed-args :default)]
        (first (filter |(not (string/find "=" $)) defaults)))))

(defn get-register-assignments
  "Get REG=VALUE assignments from positional args."
  [parsed-args]
  (if-let [defaults (parsed-args :default)]
    (parse-register-args defaults)
    @{}))

(defn print-registers
  "Print registers in a clean format, excluding :nil."
  [registers &opt prefix]
  (default prefix "")
  (def sorted-regs (sort (filter |(not= $ :nil) (keys registers))))
  (each reg sorted-regs
    (printf "%s%s: %s" prefix reg (string (registers reg)))))

(defn print-program
  "Print the compiled program instructions."
  [program]
  (print "Compiled program:")
  (eachp [i inst] (get program :instructions)
    (printf "  %3d: %s" i (string/join (map string inst) " "))))

(defn validate-register-assignments
  "Check that all registers being set actually exist in the program."
  [program assignments]
  (def available (keys (program :registers)))
  (var all-valid true)
  (eachk reg assignments
    (unless (has-key? (program :registers) reg)
      (eprintf "Warning: Register '%s' does not exist in program." reg)
      (eprintf "  Available registers: %s"
               (string/join (sort (filter |(not= $ :nil) (map string available))) ", "))
      (set all-valid false)))
  all-valid)

(defn main
  [& args]
  (def parsed (argparse ;argparse-params))
  (unless parsed (os/exit 1))

  (def fname (get-program-file parsed))
  (unless fname
    (eprint "Error: No program file specified.")
    (eprint "Usage: beancraft <file.bc> [REG=VALUE ...]")
    (eprint "       beancraft -f <file.bc> [options] [REG=VALUE ...]")
    (os/exit 1))

  # Try to open and compile the file
  (def f (file/open fname :r))
  (unless f
    (eprintf "Error: Cannot open file '%s'" fname)
    (os/exit 1))

  (def content (file/read f :all))
  (file/close f)

  (def program
    (try
      (compile content (path/dirname fname))
      ([err]
        (eprintf "Compilation error: %s" err)
        (os/exit 1))))

  # Get register assignments from CLI
  (def assignments (get-register-assignments parsed))

  # Validate register assignments
  (unless (empty? assignments)
    (validate-register-assignments program assignments))

  # Apply register assignments
  (eachp [reg val] assignments
    (when (has-key? (program :registers) reg)
      (put (program :registers) reg val)))

  # --list-registers: show registers and exit
  (when (parsed "list-registers")
    (print "Registers:")
    (print-registers (program :registers) "  ")
    (os/exit 0))

  # Print compiled program unless --quiet
  (unless (parsed "quiet")
    (print)
    (print-program program)
    (print))

  # --dry-run: don't execute
  (when (parsed "dry-run")
    (print "Registers (initial):")
    (print-registers (program :registers) "  ")
    (os/exit 0))

  # --show-jit: show generated code and exit
  (when (parsed "show-jit")
    (print "Generated JIT code:")
    (print (show-generated-code program))
    (os/exit 0))

  # Determine max steps
  (var max-steps (dyn *MAX-STEPS*))
  (when-let [max-str (parsed "max-steps")]
    (if-let [max-val (scan-number max-str)]
      (set max-steps max-val)
      (do
        (eprintf "Error: Invalid max-steps value '%s'" max-str)
        (os/exit 1))))

  # JIT execution path
  (when (parsed "jit")
    (def start-time (os/clock))
    (def result (jit-run program max-steps))
    (def elapsed (- (os/clock) start-time))

    (print "Final registers:")
    (print-registers (result :registers) "  ")

    (when (parsed "verbose")
      (print)
      (printf "Execution (JIT): %d steps in %.3f seconds" (result :steps) elapsed)
      (when (>= (result :steps) max-steps)
        (printf "  (stopped at max-steps limit: %d)" max-steps))
      (unless (result :halted)
        (print "  Warning: Program did not halt")))

    (os/exit 0))

  # Standard interpreter execution
  (var env (clone program))
  (def start-time (os/clock))
  (var steps 0)

  (while (and (not (env :halted))
              (< steps max-steps))
    (set env (step env))
    (++ steps))

  (def elapsed (- (os/clock) start-time))

  # Print results
  (print "Final registers:")
  (print-registers (env :registers) "  ")

  # Verbose: show execution stats
  (when (parsed "verbose")
    (print)
    (printf "Execution: %d steps in %.3f seconds" steps elapsed)
    (when (>= steps max-steps)
      (printf "  (stopped at max-steps limit: %d)" max-steps))
    (unless (env :halted)
      (print "  Warning: Program did not halt"))))
