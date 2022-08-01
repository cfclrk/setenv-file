# setenv-file

Set or unset environment variables from an "env" file.

This package provides two interactive functions:

1.  `setenv-file-set`: set all the env vars in an env file
2.  `setenv-file-unset`: unset all the env vars in an env file

When used interactively, each function prompts for a file. By default, the
prompt begins at `setenv-file-dir`.


# Installation

Copy `setenv-file.el` to your machine, and load it:

```emacs-lisp
(load "/path/to/setenv-file.el")
```

Or, using straight:

```emacs-lisp
(straight-use-package
 '(setenv-file :type git
	       :host github
	       :repo "cfclrk/setenv-file"))
```


# Usage

Start by creating an env file in `setenv-file-dir` (by default, `~/.env/`). For
example, create this file in `~/.env/foo`:

```sh
FOO=~/foo
BAR=$FOO/bar
ОФИС=ДОМ
BAZ=nosubst:FOO$BAR
```

Note that you can customize `setenv-file-dir` if you like, like this:

```emacs-lisp
(setq setenv-file-dir
      (expand-file-name "~/another/path/"))
```


## Interactive

Now, set environment variables in Emacs using `M-x setenv-file`, and navigate to
an env file.

View your new environment variables with `M-x getenv`.

Unset all of the variables defined in an env file.


## In elisp

To set env variables defined in `~/.env/foo`:

```emacs-lisp
(setenv-file "foo" setenv-file-dir)
```


## In org-mode

The example below shows a convenient way to declare and set environment
variables in an `org` document:

```
#+NAME: env
| Var  | Value           |
|------+-----------------|
| FOO  | ~/foo           |
| BAR  | $FOO/bar        |
| ОФИС | ДОМ             |
| BAZ  | nosubst:FOO$BAR |

#+begin_src emacs-lisp :var env=env
  (setenv-file-export-pairs env)
#+end_src
```


# File Format

Each line in the file should be in a `KEY=VALUE` format, with one entry per
line. This package does not invoke a shell to interpret the file, so most
shell-isms will not work. However, the env file may:

-   Use existing environment variables
-   Define an environment variable and use it in successive lines
-   A `~` is expanded if it is the first character in the value


# Development

1.  `make dep`: Install dependencies
2.  `make test`: Run unit tests (you must run `make dep` first!)
3.  `make doc`: Runs an org export on `doc/doc.org` which creates:
    -   `README.md`
    -   The `;;; Commentary` section in `setenv-file.el`
    -   Package texinfo (`.texi` and `.info`) in `doc/`


# See Also

-   [emacs-direnv](https://github.com/wbolster/emacs-direnv)
-   [envrc](https://github.com/purcell/envrc)
-   [parsenv](https://github.com/articuluxe/parsenv)
