# setenv-file

Set or unset environment variables from an "env" file.

This package provides an interactive function `setenv-file` to set environment
variables defined in a file. With one `C-u` prefix argument, `setenv-file`
unsets the environment variables defined in the file.

When used interactively, `setenv-file` prompts for a file, defaulting to the
directory `setenv-file-dir`.


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

Create a file with environment variable definitions. For example:

```sh
FOO=~/foo
BAR=$FOO/bar
ОФИС=ДОМ
BAZ=nosubst:FOO$BAR
```

Now, set those environment variables in Emacs using `M-x setenv-file`, and
navigate to the file. View your new environment variables with `M-x getenv`.

Optionally, set a default directory where you put such env files using
`setenv-file-dir`:

```emacs-lisp
(setq setenv-file-dir (expand-file-name "~/.env/"))
```


# Usage in org-mode

The example below shows a convenient way to declare and set environment
variables in an `org` document:

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


# File Format

Each line in the file should be in a `KEY=VALUE` format, with one entry per
line. This package does not invoke a shell to interpret the file, so most
shell-isms will not work. However, the env file may:

-   Use existing environment variables
-   Define an environment variable and use it in successive lines
-   A `~` is expanded if it is the first character in the value


# Development

1.  `make dep`: Install dependencies
2.  `make test`: Run unit tests
3.  `make doc`: Runs an org export on `doc/doc.org` which creates:
    -   `README.md`
    -   The `;;; Commentary` section in `setenv-file.el`
    -   Package texinfo (`.texi` and `.info`) in `doc/`


# See Also

-   [emacs-direnv](https://github.com/wbolster/emacs-direnv)
-   [envrc](https://github.com/purcell/envrc)
-   [parsenv](https://github.com/articuluxe/parsenv)
