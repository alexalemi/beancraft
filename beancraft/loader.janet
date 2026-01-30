# Module Loader for Beancraft
#
# Handles loading of beancraft modules with:
# - Multiple search paths (BEANCRAFT_PATH environment variable)
# - Standard library location (~/.beancraft/lib/)
# - Module caching (don't re-parse the same file)
# - Better error messages showing search locations

(use spork)

# Default locations
(def DEFAULT-LIB-PATH (path/join (os/getenv "HOME" "") ".beancraft" "lib"))
(def BEANCRAFT-ROOT (os/getenv "BEANCRAFTROOT" (path/join (os/getenv "HOME" "") ".beancraft")))

# Module cache - maps absolute paths to parsed content
(var *module-cache* @{})

(defn clear-cache
  "Clear the module cache."
  []
  (set *module-cache* @{}))

(defn get-search-paths
  "Get the list of directories to search for modules.

   Search order:
   1. The directory containing the main file (if provided)
   2. Paths from BEANCRAFT_PATH environment variable (colon-separated)
   3. Standard library path (~/.beancraft/lib/)
   4. BEANCRAFTROOT fallback"
  [&opt base-path]
  (def paths @[])

  # 1. Base path (directory of the importing file)
  (when base-path
    (array/push paths base-path))

  # 2. BEANCRAFT_PATH environment variable
  (when-let [env-path (os/getenv "BEANCRAFT_PATH")]
    (each p (string/split ":" env-path)
      (when (not (empty? p))
        (array/push paths p))))

  # 3. Standard library path
  (array/push paths DEFAULT-LIB-PATH)

  # 4. BEANCRAFTROOT fallback
  (array/push paths BEANCRAFT-ROOT)

  paths)

(defn resolve-module-path
  "Resolve a module name to an absolute file path.

   Supports:
   - Relative paths starting with './' or '../'
   - Absolute paths starting with '/'
   - Module names (searched in search paths)

   Returns [resolved-path searched-paths] or [nil searched-paths] if not found."
  [module-name &opt base-path]
  (def searched @[])

  # Handle .bc extension
  (def filename (if (string/has-suffix? ".bc" module-name)
                  module-name
                  (string module-name ".bc")))

  # Check for absolute path
  (when (string/has-prefix? "/" filename)
    (array/push searched filename)
    (if (os/stat filename)
      (break [filename searched])
      (break [nil searched])))

  # Check for relative path (starts with ./ or ../)
  (when (or (string/has-prefix? "./" filename)
            (string/has-prefix? "../" filename))
    (when base-path
      (def resolved (path/join base-path filename))
      (def normalized (path/normalize resolved))
      (array/push searched normalized)
      (if (os/stat normalized)
        (break [normalized searched])
        (break [nil searched]))))

  # Search in search paths
  (def search-paths (get-search-paths base-path))
  (each search-path search-paths
    (def candidate (path/join search-path filename))
    (def normalized (path/normalize candidate))
    (array/push searched normalized)
    (when (os/stat normalized)
      (break [normalized searched])))

  [nil searched])

(defn load-module-source
  "Load module source code from a file path.
   Uses cache if available."
  [filepath]
  (if-let [cached (get *module-cache* filepath)]
    cached
    (let [content (slurp filepath)]
      (put *module-cache* filepath content)
      content)))

(defn format-search-error
  "Format a helpful error message when a module is not found."
  [module-name searched-paths]
  (def msg @["Module '"])
  (array/push msg module-name)
  (array/push msg "' not found.\n")
  (array/push msg "Searched in:\n")
  (each p searched-paths
    (array/push msg "  - ")
    (array/push msg p)
    (array/push msg "\n"))
  (array/push msg "\nTo add search paths, set BEANCRAFT_PATH environment variable:\n")
  (array/push msg "  export BEANCRAFT_PATH=\"/path/to/modules:/another/path\"\n")
  (string/join msg ""))

(defn load-module
  "Load a beancraft module by name.

   Returns the source code of the module.
   Throws an error with helpful message if not found."
  [module-name &opt base-path]
  (let [[resolved searched] (resolve-module-path module-name base-path)]
    (if resolved
      (do
        (load-module-source resolved))
      (error (format-search-error module-name searched)))))

(defn get-module-dir
  "Get the directory containing a resolved module.
   Used for resolving nested imports relative to the importing file."
  [module-name &opt base-path]
  (let [[resolved _] (resolve-module-path module-name base-path)]
    (when resolved
      (path/dirname resolved))))

(defn module-exists?
  "Check if a module can be resolved."
  [module-name &opt base-path]
  (let [[resolved _] (resolve-module-path module-name base-path)]
    (truthy? resolved)))

(defn list-search-paths
  "Print the current search paths for debugging."
  [&opt base-path]
  (print "Module search paths:")
  (each p (get-search-paths base-path)
    (def exists (if (os/stat p) "✓" "✗"))
    (printf "  %s %s" exists p)))
