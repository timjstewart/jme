# jme (Java Mode Extensions)

Makes Java development a little more easy and fun.

There are still a lot of rough edges but I think it's to the point where it's
useful.

## Setup

```
(require 'flycheck)
(global-flycheck-mode t)

(require 'jme)
(jme)
```

## Customization

```
(customize-group 'jme)
```

## Commands

* jme-check-file-style - run checkstyle on the current file (requires
  checkstyle executable in PATH environment variable).
* jme-compile-current-file - compile the current file (saving any
  unsaved changes).
* jme-compile-project - runs 'mvn clean compile' on the project.
* jme-copy-import-statement - copies an import statement that will
  import the current file.
* jme-current-class-name-fully-qualified - gets the fully qualified
  name of the current class.
* jme-current-package-name - gets the current file's package name.
* jme-edit-pom-file - edits the current project's pom.xml file.
* jme-generate-javadoc - generates JavaDoc HTML documentation and
  opens the index in your browser.
* jme-install-checker - installs the Java flychecker.
* jme-javap-file - disassemble the current Java source file (requires
  javap-mode).
* jme-new-project - creates a new Maven project.
* jme-rebuild-classpath-cache - clears the CLASSPATH cache and
  rebuilds it.  Also installs updated flychecker.
* jme-run-maven - runs one or more Maven commands that the user
  specifies.
* jme-run-tests - runs unit tests.
* jme-sort-imports - sort import statements lexicographically and by
  user-customizable order.
* jme-yank-import-statement - yanks the text at the top of the kill
  ring and, if it's an import, inserts at the top of the file and
  re-organizes the import statements.

## Goes Great With

* checkstyle
* imenu
* javap-mode
* magit
* projectile
* yasnippet
