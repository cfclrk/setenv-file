# setenv-file

Set or unset environment variables from a file.

This package provides an interactive function `setenv-file` to set environment
variables defined in a file. With one `C-u` prefix argument, `setenv-file`
unsets the environment variables defined in the file.

When used interactively, `setenv-file` prompts for a file, defaulting to the
directory `setenv-file-dir`.


# Usage

Load `setenv-file.el` in Emacs, and optionally set a `setenv-file-dir`:

```emacs-lisp
(load "/path/to/setenv-file.el")
(setq setenv-file-dir (expand-file-name "~/.env/"))
```

Create a file in `setenv-file-dir` with environment variable definitions. For
example:

```sh
FOO=~/foo
BAR=$FOO/bar
ОФИС=ДОМ
```

And now you can set those environment variables in Emacs using `M-x
setenv-file`, and navigate to the file. View your new environment variables with
`M-x getenv`.


# Usage in org-mode

Set Emacs environment variables from an org mode document by evaluating the
`src` block below (with `C-c C-c`):

    #+NAME: env
    | Var  | Value    |
    |------+----------|
    | FOO  | ~/foo    |
    | BAR  | $FOO/bar |
    | ОФИС | ДОМ      |
    
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
