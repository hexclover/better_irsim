better_irsim
====

Dependencies
----

- ocaml >= 4.14
- dune
- ppx_deriving
- angstrom

Build & Run
----

This will build the default targets (the library and the CLI program):

```console
$ dune build
```

This will run the CLI program:

```console
$ dune exec irsim_cli [args]
```

This will install the program to the home directory (the binary will be called `irsim_cli`):

```console
$ dune build @install
$ dune install
```

For usage and options see `irsim_cli -help`.

The GUI is being worked on.

TODO
----

- More options: Limit time / number of instructions to run; trap; etc.
- Document the syntax
- Better parsing error messages
- Closely imitate the original behavior wrt immediates and arithmetic
