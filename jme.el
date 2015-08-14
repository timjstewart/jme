;;; package -- jme - java-mode extensions

;;; Commentary:

;;;=============================================================================
;;; Code:
;;;=============================================================================

(require 'flycheck)
(require 'javap-mode)
(require 's)
(eval-when-compile
  (require 'cl))

;;==============================================================================
;; Constants
;;==============================================================================

(defconst +jme-pom-file+ "pom.xml"
  "The name of the Maven Project Object Model file.")

(defconst +jme-cache-file-name+ ".jme-cache"
  "The name of the Java CLASSPATH cache file.

This file, if it exists, is located in the same directory as the
pom.xml file.")

(defconst +jme-output-buffer-name+ "*JME Output*"
  "The name of the buffer where output is written.")

(defconst +jme-source-directory-regexp+
  "/src/\\(main\\|test\\)/\\(scala\\|java\\)/\\(.*\\)/")

;;==============================================================================
;; Customizable Variables
;;==============================================================================

(defcustom jme-checkstyle-file nil
  "The checkstyle style file to use."
  :group 'jme)

(defcustom jme-debug-mode nil
  "Set to t to debug problems."
  :group 'jme)

(defcustom jme-show-output-buffer t
  "Set to t to show command output or nil not to."
  :group 'jme)

(defcustom jme-maven-home "/usr/local/"
  "The path to where Maven is installed.

When mvn is executed, it will be run from the bin directory
  underneath this directory."
  :group 'jme)

(defcustom jme-java-home "/usr/"
  "The path to where Java is installed."
  :group 'jme)

(defcustom jme-package-order
  '("java.lang." "java.util." "java.io." "java.nio." "java.")
  "Define an order for packages."
  :group 'jme)

;;==============================================================================
;; Customization Group
;;==============================================================================

(defgroup jme nil
  "Package of Java Development utilities."
  :prefix "jme-"
  :group 'languages)

;;==============================================================================
;; Commands
;;==============================================================================


(defun jme-edit-pom-file ()
  "Edit the pom.xml file in a buffer."
  (interactive)
    (let ((project-directory (jme-find-project-directory default-directory)))
      (if project-directory
          (find-file (jme--get-pom-file-path project-directory))
        (error "Not in a Maven project: %s" default-directory))))

;;------------------------------------------------------------------------------

(defun jme-yank-import-statement ()
  "Insert the import statement the top of the kill ring."
  (interactive)
  (save-excursion
    (let ((import (s-trim (substring-no-properties (current-kill 0 t)))))
      (if (s-starts-with? "import" import)
          (progn
            (goto-char (point-min))
            (search-forward-regexp "^package")
            (next-line 2)
            (beginning-of-line)
            (yank)
            (jme-sort-imports))
        (error "Not an import statement: %s" import)))))

;;------------------------------------------------------------------------------

(defun jme-rebuild-classpath-cache ()
  "Force the CLASSPATH cache to be rebuilt."
  (interactive)
  (let ((project-directory (jme-find-project-directory default-directory)))
    (if project-directory
        (progn
          (jme-clear-classpath-cache project-directory)
          (jme-get-classpath project-directory)
          (jme-install-checker))
      (error "Not in a Maven project: %s" default-directory))))

;;------------------------------------------------------------------------------

(defun jme-generate-javadoc ()
  "Generate JavaDoc documentation for the current project."
  (interactive)
  (let ((project-directory (jme-find-project-directory default-directory)))
    (if project-directory
        (progn
          (save-some-buffers)
          (message "Generating Documentation...")
          (jme--run-command (concat jme-maven-home "bin/mvn"  " -o" " javadoc:javadoc")
                            project-directory)
          (message "Done.  Launching Browser...")
          (browse-url (concat "file://"
                              (expand-file-name project-directory)
                              "target/site/apidocs/index.html")))
      (error "Not in a Maven project: %s" default-directory))))
  
;;------------------------------------------------------------------------------

(defun jme-run-maven (commands)
  "Run one or more Maven COMMANDS."
  (interactive "sEnter commands: ")
  (let ((project-directory (jme-find-project-directory default-directory)))
    (if project-directory
        (progn
          (save-some-buffers)
          (jme--run-maven-goals
           (s-split " " commands) project-directory
           :banner (format "Running 'mvn %s' in %s..." commands project-directory)))
      (error "Not in a Maven project: %s" default-directory))))
  
;;------------------------------------------------------------------------------

(defun jme-check-file-style ()
  "Run checkstyle on a Maven project."
  (interactive)
  (if (not jme-checkstyle-file)
      (error "Please customize jme-checkstyle-file before running"))
  (if (not (buffer-file-name))
      (error "Current buffer is not visiting a file"))
  (save-buffer)
  (jme--run-command-with-output "checkstyle"
                                :args (list "-c" jme-checkstyle-file (buffer-file-name))
                                :compilation t
                                :banner (format "Checking Style: %s"
                                                (buffer-file-name))))

;;------------------------------------------------------------------------------

(defun jme-current-package-name ()
  "Return the current package name."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if file-name
        (let ((matches (s-match +jme-source-directory-regexp+ file-name)))
          (if matches
              (let ((dirs (nth 3 matches)))
                (s-replace "/" "." dirs))
            nil))
      (error "Buffer has no file name"))))

;;------------------------------------------------------------------------------

(defun jme-current-class-name-fully-qualified ()
  "Return the fully qualified name of the current class."
  (interactive)
  (concat (jme-current-package-name) "."
          (file-name-nondirectory (file-name-sans-extension (buffer-file-name)))))

;;------------------------------------------------------------------------------

(defun jme-copy-import-statement ()
  "Copy an import directive that imports the current file."
  (interactive)
  (kill-new (concat "import " (jme-current-class-name-fully-qualified) ";\n")))

;;------------------------------------------------------------------------------

(defun jme-sort-imports ()
  "Sort imports lexicographically and by order in jme-package-order."
  (interactive)
  (cl-flet ((find-index (import)
                        (position-if (lambda (package-prefix)
                                       (s-contains-p package-prefix import))
                                     jme-package-order))
            (compare (a ai b bi)
                     (cond ((and ai bi (= ai bi)) (string-lessp a b))
                            ((and ai bi) (< ai bi))
                            (ai t)
                            (bi nil)
                            (t (string-lessp a b)))))
    (save-excursion
      (let* ((imports (list "")) ; This has the effect of dividing imports into
                                 ; two groups: those in jme-package-order, and
                                 ; those that are not.
             (first-import-point nil)
             (last-import-point nil))
        (goto-char (point-min))
        (while (search-forward-regexp "^import " nil t)
          (unless first-import-point
            (setq first-import-point (line-beginning-position)))
          (let ((import (buffer-substring (line-beginning-position)
                                          (line-end-position))))
            (push import imports)
            (setq last-import-point (line-end-position))))
        (setq imports
              (sort imports (lambda (a b)
                              (let ((ai (find-index a))
                                    (bi (find-index b)))
                                (compare a ai b bi)))))
        (delete-region first-import-point last-import-point)
        (goto-char first-import-point)
        (insert (s-join "\n" (remove-duplicates imports :test #'string-equal)))
        (delete-blank-lines)
        (if (not (looking-at "^$"))
            (insert "\n"))))))

;;------------------------------------------------------------------------------

(defun jme-install-checker ()
  "Install a flychecker given a DIRECTORY in the project."
  (interactive)
  (let* ((project-directory (jme-find-project-directory default-directory))
         (classpath (jme-get-classpath project-directory))
         (javac-path (concat jme-java-home "/bin/javac")))
    (flycheck-define-command-checker
        'jme "A flycheck checker for Java"
        :command (list javac-path
                       "-Xlint:all"
                       "-classpath"
                       classpath
                       'source)
        :error-patterns '((error line-start (1+ nonl) ":" line ":" (message) line-end))
        :modes 'java-mode)
    (add-to-list 'flycheck-checkers 'jme)))

;;------------------------------------------------------------------------------

(defun jme ()
  "Turn on all the jme features."
  (interactive)
  (if (symbolp projectile-switch-project-hook)
      (add-hook 'projectile-switch-project-hook
                (lambda ()
                  (message "Switching Projects...")
                  (if (jme-find-project-directory default-directory)
                      (jme-install-checker))))))

;;------------------------------------------------------------------------------

(defun jme-javap-file ()
  "Disassemble current file using javap-mode."
  (interactive)
  (let ((class-file (jme--find-class-file (buffer-file-name))))
    (if (not class-file)
        (error "Is %s in the standard directory layout?" (buffer-file-name)))
    (let ((buffer (find-buffer-visiting class-file)))
      (if (not (file-exists-p class-file))
          (error "Class file %s does not exist" class-file))
      (if buffer
          (with-current-buffer buffer
            (javap-buffer))
        (find-file class-file)))))

;;------------------------------------------------------------------------------

(defun jme-compile-current-file ()
  "Compiles the current source file.

This function is not very useful if the save hook is installed
because saving the file (something this function does) triggers
compilation if the save hook is installed."
  (interactive)
  (jme--require-buffer-file)
  (save-buffer)
  (message "Compiling %s..." (buffer-file-name))
  (jme--compile-file (buffer-file-name))
  (message "Done."))

;;------------------------------------------------------------------------------

(defun jme-compile-project ()
  "Compile the current Maven project.

The user is prompted to save any modified buffers."
  (interactive)
  (let ((project-directory (jme-find-project-directory default-directory)))
    (if project-directory
        (progn
          (save-some-buffers)
          (jme--run-maven-goals
           (list "clean" "compile") project-directory
           :banner (format "Compiling project: %s..." project-directory)))
      (error "Not in a Maven project: %s" default-directory))))

;;------------------------------------------------------------------------------

(defun jme-run-tests ()
  "Run all of the test suites in the current project."
  (interactive)
  (let ((project-directory (jme-find-project-directory default-directory)))
    (if project-directory
        (jme--run-maven-goals '("test") project-directory
                              :banner "Executing Tests...")
      (error "Not in a Maven project: %s" default-directory))))
;;------------------------------------------------------------------------------

(defun jme-new-project (directory group-id artifact-id)
  "Create a new Maven project in DIRECTORY with the given GROUP-ID and ARTIFACT-ID."
  (interactive "Ddirectory: \nsgroup-id: \nsartifact-id: ")
  (if (or (not (jme-find-project-directory directory))
          (y-or-n-p (format "%s is already in a Maven project.  Continue?"
                                  directory)))
      (jme--run-maven-goals (list "-B" "archetype:generate"
                                  "-DarchetypeGroupId=org.apache.maven.archetypes"
                                  (format "-DgroupId=%s" group-id)
                                  (format "-DartifactId=%s" artifact-id))
                            directory
                            :banner "Creating project...")))

;;==============================================================================
;; Public Functions
;;==============================================================================

(defun jme-find-project-directory (current-directory)
  "Find the Maven project directory starting in CURRENT-DIRECTORY.

Starts search for a Maven project directory in CURRENT-DIRECTORY
and if no pom.xml file is found there, the current directory's
parent directories are searched in turn until the pom.xml file is
found or the root of the file system is reached.

Return the project directory if it's found; otherwise nil is
returned."
  (if (and current-directory
           (not (string-equal "/" current-directory)))
      (let ((pom-file-path (jme--get-pom-file-path current-directory)))
        (if (and pom-file-path
                 (file-exists-p pom-file-path))
            current-directory
          (jme-find-project-directory
           (file-name-directory (directory-file-name current-directory)))))))

;;------------------------------------------------------------------------------

(defun jme-clear-classpath-cache (project-directory)
  "Deletes the classpath cache file in PROJECT-DIRECTORY."
  (let ((cache-file-path (jme--get-cache-file-path project-directory)))
    (if (file-exists-p cache-file-path)
        (delete-file cache-file-path))))

;;------------------------------------------------------------------------------

(defun jme-build-classpath (project-directory)
  "Return the Java CLASSPATH for the Maven project in PROJECT-DIRECTORY."
  (cl-flet ((should-filter (line)
                           (or
                            (s-starts-with? "[INFO]" line)
                            (s-starts-with? "Downloaded" line)
                            (s-starts-with? "Downloading" line))))
    (let ((command (concat "JAVA_HOME=" jme-java-home
                           " " jme-maven-home "bin/mvn dependency:build-classpath")))
      (message "Building CLASSPATH.  This may take a while...")
      (concat (expand-file-name (file-name-as-directory project-directory))
                      "target/classes" ":"
                      (apply #'concat (cl-remove-if #'should-filter
                                    (jme--run-command command project-directory)))))))

;;------------------------------------------------------------------------------

(cl-defun jme-get-classpath (current-directory &key (rebuild nil))
  "Return the CLASSPATH for the project that owns CURRENT-DIRECTORY.

First checks in classpath cache.  If not present, builds the
classpath, and stores it in the classpath cache file."
  (let* ((project-directory (jme-find-project-directory current-directory))
         (classpath (jme--read-classpath-cache project-directory)))
    (or classpath
        (jme--write-classpath-cache (jme-build-classpath project-directory)
                                    project-directory))))

;;------------------------------------------------------------------------------

(defun jme-install-save-hook ()
  "Install a 'after-save-hook' that compiles Java source files."
  (add-hook 'after-save-hook
            (lambda ()
              (let ((file-name (buffer-file-name)))
                (if (string-equal "java" (file-name-extension file-name))
                    (jme--compile-file (buffer-file-name)))))))

;;==============================================================================
;; Private Functions
;;==============================================================================

(defun jme--get-pom-file-path (directory)
  "Return the concatenation of DIRECTORY and pom.xml.

Note: The returned value may not name an existing file.  In fact,
this function is mostly used when trying to see if the pom.xml is
in a particular directory."
  (if directory
      (let ((parent-directory (file-name-as-directory directory)))
        (if parent-directory
            (concat parent-directory +jme-pom-file+)
          nil))))

;;------------------------------------------------------------------------------

(defun jme--get-cache-file-path (directory)
  "Return the concatenation of DIRECTORY and the cache file.

Note: The returned value may not name an existing file.  In fact,
this function is mostly used when trying to see if the classpath
cache file exists."
  (concat (file-name-as-directory directory) +jme-cache-file-name+))

;;------------------------------------------------------------------------------

(defun jme--read-classpath-cache (project-directory)
  "Return the cached CLASSPATH stored in PROJECT-DIRECTORY.

If the cache file does not exist, return nil."
  (let ((cache-file-path (jme--get-cache-file-path project-directory)))
    (if (not (jme--is-cache-stale project-directory))
        (with-temp-buffer
          (insert-file-contents cache-file-path)
          (s-join ":" (s-split "\n" (s-trim (buffer-string))))))))

;;------------------------------------------------------------------------------

(defun jme--is-cache-stale (project-directory)
  "Return t iff the cache file in PROJECT-DIRECTORY is stale.

If the cache file does not exist, it is considered stale."
  (let* ((cache-file-path (jme--get-cache-file-path project-directory))
         (pom-file-path (jme--get-pom-file-path project-directory)))
    (or (not (file-exists-p cache-file-path))
        (file-newer-than-file-p pom-file-path cache-file-path))))

;;------------------------------------------------------------------------------

(defun jme--write-classpath-cache (classpath project-directory)
  "Write CLASSPATH to the cache file in PROJECT-DIRECTORY.

Return the classpath that was written to the classpath cache
file."
  (let ((cache-file-path (concat (file-name-as-directory project-directory)
                                 +jme-cache-file-name+)))
    (if (or (file-writable-p cache-file-path)
            (not (file-exists-p cache-file-path)))
        (with-temp-buffer
          (insert (s-join "\n" (s-split ":" classpath)))
          (write-region (point-min) (point-max)
                        cache-file-path)
          classpath)
      (error "Error: jme classpath cache file is not writeable: %s"
             cache-file-path))))

;;------------------------------------------------------------------------------

(defun jme--compile-file (file-path)
  "Compile source Java file named FILE-PATH.

Upon success the .class file is stored in a package specific
subdirectory of target/classes."
  (let* ((file-directory (file-name-directory (directory-file-name file-path)))
         (project-directory (jme-find-project-directory file-directory))
         (classpath (jme-get-classpath project-directory))
         (buffer (get-buffer-create +jme-output-buffer-name+))
         (command (concat (file-name-as-directory jme-java-home) "bin/javac"))
         (banner (concat "Compiling: " file-path "\n"
                         "CLASSPATH: " classpath "\n")))
    (jme--run-command-with-output command
                                  :directory project-directory
                                  :compilation t
                                  :banner banner
                                  :args (list "-Xlint:all"
                                              "-Xlint:-path"
                                              "-d" (concat project-directory "target/classes")
                                              "-classpath" classpath
                                              file-path))))

;;------------------------------------------------------------------------------

(defun jme--require-buffer-file ()
  "Raise error if current buffer is not visiting a file."
    (if (not (buffer-file-name))
      (error "The buffer '%s' is not visiting a file" (buffer-name))))

;;------------------------------------------------------------------------------

(cl-defun jme--run-maven-goals (goals project-directory &key (banner nil))
  "Run the specified Maven GOALS on the project in PROJECT-DIRECTORY."
  (setenv "JAVA_HOME" jme-java-home)
  (jme--run-command-with-output (concat jme-maven-home "bin/mvn")
                                :directory project-directory
                                :display 'always
                                :banner banner
                                :compilation t
                                :args goals))

;;------------------------------------------------------------------------------

(defun jme--run-command (command project-directory)
  "Run COMMAND in the PROJECT-DIRECTORY directory."
  (let ((default-directory (file-name-as-directory project-directory)))
    (s-split "\n" (shell-command-to-string command) t)))

;;-------------------------------------------------------------------------------

(defun jme--install-checker (project-directory)
  "Install a flychecker for Java source in PROJECT-DIRECTORY."
  (let* ((classpath (jme-get-classpath project-directory))
         (javac-path (concat jme-java-home "/bin/javac")))
    (flycheck-define-command-checker 'jme
      "A flycheck checker for Java"
      :command (list javac-path
                     "-Xlint:all"
                     ;"-Xdiags:verbose"
                     "-classpath"
                     classpath
                     'source)
      :error-patterns '((error line-start (1+ nonl) ":" line ":" (message) line-end))
      :modes 'java-mode)
    (add-to-list 'flycheck-checkers 'jme)))

;;------------------------------------------------------------------------------

(defun jme--find-class-file (source-file-path)
  "Return the .class file for SOURCE-FILE-PATH."
  (let* ((source-file-directory (file-name-directory (directory-file-name source-file-path)))
         (project-directory (jme-find-project-directory source-file-directory))
         (relative-path (substring source-file-path (length project-directory))))
    (when (s-starts-with? "src/main/java/" relative-path)
      (setq relative-path (substring relative-path (length "src/main/java/")))
      (concat (file-name-as-directory project-directory)
              "target/classes/"
              (file-name-sans-extension relative-path)
              ".class"))))

;;------------------------------------------------------------------------------

(cl-defun jme--run-command-with-output (command &key (args nil) (directory nil)
                                                (banner nil) (compilation nil) (display 'on-error))
  "Run COMMAND and write its output to output buffer."
  (let ((buffer (get-buffer-create +jme-output-buffer-name+)))
    (cl-flet ((sentinel (process event)
                        (let ((buffer (process-buffer process)))
                          (with-current-buffer buffer
                            (if (string-equal event "finished\n")
                                (insert "Status: Success.\n")
                              (insert "\nStatus: Failed.\n"))
                            (if (not (string-equal event "finished\n"))
                                (pop-to-buffer buffer))
                            (shrink-window-if-larger-than-buffer
                             (get-buffer-window (current-buffer)))))))
      (with-current-buffer buffer
        (let ((default-directory (or directory default-directory)))
          (if compilation
              (compilation-mode)
            (fundamental-mode))
          (setq buffer-read-only nil)
          (delete-region (point-min) (point-max))
          (if banner
              (insert banner "\n"))
          (if (eq display 'always)
              (pop-to-buffer buffer))
          (let ((process (apply #'start-process "jme-process"
                                buffer command args)))
            (set-process-sentinel process #'sentinel)))))))

;;==============================================================================
;; Todo
;;==============================================================================

;; Make Javap polite for unit test files that are in a different directory.
;; Turn into minor mode?

;;==============================================================================
;; Done
;;==============================================================================

;; Generate JavaDoc
;; Generate New Project
;; Run any Maven goals
;; Consolidate functions that have a command version and a function it delegates to.
;; Add projectile project switch hook that defines a new flycheck-checker.
;; Improve signature of functions that run shell commands.
;; Write to output buffer asynchronously.
;; Only display output window on error (or debug).
;; Filter out non-existent jar files in classpath.
;; Turn on compilation-mode in output buffer.
;; Make sure that you can write to the output buffer after it's in compilation mode.
;; Do we need to set JAVA_HOME when running javac?
;; Disassemble Java File (javap).
;; sort imports.
;; Rename project to something more general.

(provide 'jme)
;;; jme.el ends here
