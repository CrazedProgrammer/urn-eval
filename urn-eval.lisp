(import urn/tools/docs       docs)
(import urn/tools/repl       repl)
(import urn/tools/gen-native gen-native)
(import urn/tools/run        run)
(import urn/tools/simple     simple)

(import urn/analysis/optimise optimise)
(import urn/analysis/warning warning)
(import urn/backend/lua lua)
(import urn/error error)
(import urn/loader loader)
(import urn/library library)
(import urn/logger logger)
(import urn/logger/term term)
(import urn/luarocks luarocks)
(import urn/plugins plugins)
(import urn/resolve/builtins builtins)
(import urn/resolve/scope scope)
(import urn/resolve/state state)
(import urn/timer timer)
(import urn/traceback traceback)

(import io io)
(import io/argparse arg)
(import lua/basic (_G loadfile))
(import lua/debug debug)
(import lua/os os)

(define function-cache :hidden {})

(defun normalise-path (path trailing) :hidden
  "Normalise the given file PATH. If TRAILING is `true`, then a trailing
   '/' will be added if required."
  :hidden
  ;; Normalise path separators
  (set! path (string/gsub path "\\" "/"))

  ;; Add a trailing "/" where needed
  (when (and trailing (/= path "") (/= (string/char-at path -1) "/"))
    (set! path (.. path "/")))

  ;; Remove leading "./"s
  (while (= (string/sub path 1 2) "./")
    (set! path (string/sub path 3)))

  path)

(defun eval-string (code lib-dir) :hidden
  (with (tmp-path (os/tmpname))
    (io/write-all! (.. tmp-path ".lisp") code)
    (let* [(spec (arg/create "The compiler and REPL for the Urn programming language."))
           ;; Attempt to derive the directory the compiler lives in
           (directory (.. lib-dir "/"))

           ;; Determine whether we should use urn-lib or lib directories.
           (lib-name (with (handle (io/open (.. directory "urn-lib/prelude.lisp")))
                       (cond
                         [handle
                          (self handle :close)
                          "urn-lib"]
                         [else "lib"])))

           ;; Build a list of search paths from this information
           (paths (list
                    "?"
                    "?/init"
                    (.. directory lib-name "/?")
                    (.. directory lib-name "/?/init")))

           ;; Build a list of tasks to run
           (tasks (list
                    ;; Must be done before any processing of the tree
                    run/coverage-report
                    docs/task
                    gen-native/task
                    ;; Various processing steps
                    simple/warning
                    simple/optimise
                    ;; Then using the fully optimised result
                    simple/emit-lua
                    ))]

      ;; Setup the arguments for each task
      (for-each task tasks ((.> task :setup) spec))

      (let* [(args {
               :optimise-n 10
               :include '()
               :explain false
               :flag '()
               :optimise 1
               :include-rocks false
               :gen-native false
               :warning 1
               :optimise-time -1
               :shebang false
               :input (list tmp-path)
               :emit-lua true
               :wrapper false
               :time 0
               :prelude (.. directory lib-name "/prelude.lisp")
               :verbose 0
               :chmod false
               :output tmp-path
               :plugin (list (.. directory "plugins/fold-defgeneric.lisp"))

             })
             (logger (term/create
                       (.> args :verbose)
                       (.> args :explain)
                       (.> args :time)))]

        ;; Process include paths
        (for-each path (.> args :include)
          (cond
            [(string/find path "%?") (push! paths (normalise-path path false))]
            [else
             (set! path (normalise-path path true))
             (push! paths (.. path "?"))
             (push! paths (.. path "?/init"))]))

        ;; Include LuaRocks modules
        (when (.> args :include-rocks)
          (luarocks/include-rocks logger paths))

        (logger/put-verbose! logger (.. "Using path: " (pretty paths)))

        (when (and (= (.> args :prelude) (.. directory lib-name "/prelude")) (empty? (.> args :plugin)))
          (push! (.> args :plugin) (.. directory "plugins/fold-defgeneric.lisp")))

        (cond
          [(empty? (.> args :input))
           (.<! args :repl true)]
          [(not (.> args :emit-lua))
           (.<! args :emit-lua true)]
          [else])

        (with (compiler { :log       logger
                          :timer     (timer/create (cut logger/put-time! logger <> <> <>))
                          :paths     paths
                          :flags     (.> args :flag)

                          :libs       (library/library-cache)
                          :prelude    nil
                          :root-scope builtins/root-scope

                          :warning   (warning/default)
                          :optimise  (optimise/default)

                          :exec      (lambda (func) (list (xpcall func traceback/traceback)))

                          :variables {}
                          :states    {}
                          :out       '() })

          ;; Add compileState
          (.<! compiler :compile-state (lua/create-state))

          ;; Set the loader
          (.<! compiler :loader (cut loader/named-loader compiler <>))

          ;; Add globals
          (.<! compiler :global (setmetatable
                                  {  :_libs (library/library-cache-values (.> compiler :libs))
                                    :_compiler (plugins/create-plugin-state compiler)}
                                  { :__index _G }))

          ;; Store all builtin vars in the lookup
          (for-pairs (_ var) (scope/scope-variables builtins/root-scope)
            (.<! compiler :variables (tostring var) var))

          ;; Run the various setup functions
          (for-each task tasks
            (when ((.> task :pred) args)
              (when-with (setup (.> task :init)) (setup compiler args))))

          (timer/start-timer! (.> compiler :timer) "loading")
          (with (do-load! (lambda (name)
                            (case (list (xpcall (lambda () (loader/path-loader compiler name)) error/traceback))
                              ;; Could not resolve any nodes, so just do a pure exit
                              [(false error/compiler-error?) (exit! 1)]
                              ;; Some unknown crash, so fail with that.
                              [(false ?error-message)  (fail! error-message)]
                              ;; Module not found
                              [(true (nil ?error-message))
                               (logger/put-error! logger error-message)
                               (exit! 1)]
                              [(true (?lib)) lib])))

            ;; Load the prelude and setup the environment
            (with (lib (do-load! (.> args :prelude)))
              (loader/setup-prelude! compiler lib))

            ;; And load remaining files
            (for-each input (append (.> args :plugin) (.> args :input))
              (do-load! input)))

          (timer/stop-timer! (.> compiler :timer) "loading")

          (for-each task tasks
            (when ((.> task :pred) args)
              (timer/start-timer! (.> compiler :timer) (.> task :name) 1)
              ((.> task :run) compiler args)
              (timer/stop-timer! (.> compiler :timer) (.> task :name)))))))
    (with (func (loadfile (.. tmp-path ".lua")))
      (os/remove tmp-path)
      (os/remove (.. tmp-path ".lisp"))
      (os/remove (.. tmp-path ".lua"))
      func)))

(defun function-string (code) :hidden
  (case (type code)
    ["string" code]
    ["list" (pretty code)]
    [true (error! (.. "Cannot eval type " (type code)))]))

(defun eval-raw (code lib-path)
  (eval-string (function-string code) lib-path))

(defun eval (code lib-path)
  (let* [(code-str (function-string code))
         (function-name (.. code-str lib-path))]
    (when (not (.> function-cache function-name))
      (.<! function-cache function-name (eval-raw code lib-path)))
    (.> function-cache function-name)))

(defun clear-cache! ()
  (for-each key (keys function-cache)
    (.<! function-cache key nil)))
