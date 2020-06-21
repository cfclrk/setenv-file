# setenv-file

Set or unset environment variables from a file.

This package provides an interactive function `setenv-file` to set environment
variables defined in a file. With one `C-u` prefix argument, `setenv-file`
unsets the environment variables defined in the file.

When used interactively, `setenv-file` prompts for a file, defaulting to the
directory `setenv-file-dir`.


# How To

Create a file with environment variable definitions. For example:

```sh
FOO=~/foo
BAR=$FOO/bar
ОФИС=ДОМ
```

Set those environment variables in Emacs using `M-x setenv-file`, and navigate
to the file. View your new environment variables with `M-x getenv`.

To customize the default directory where you keep your env files:

```emacs-lisp
(setq setenv-file-dir (expand-file-name "~/.env/"))
```


# File Format

Each line in the file should be in a `KEY=VALUE` format, with one entry per
line. This package does not invoke a shell to interpret the file, so most
shell-isms will not work. However, the env file may:

-   Use existing environment variables
-   Define and environment variable and use it in successive lines
-   Tildes are expanded if they are the first character of the value


# Development

1.  Install dependencies: `make install`
2.  Run tests: `make test`
3.  Generate documentation: `make docs`


# See Also

-   [parsenv](https://github.com/articuluxe/parsenv)
-   [emacs-direnv](https://github.com/wbolster/emacs-direnv)
-   [projectile-direnv](https://github.com/christianromney/projectile-direnv)
