better_irsim
====

Dependencies
----

- ocaml >= 4.14
- dune
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

This will install the program to the home directory (the binary is called `irsim_cli`):

```console
$ dune build @install
$ dune build install
```

The GUI is being worked on.

TODO
----

- More options: Limit time / number of instructions to run; trap; etc.
- Document the syntax
- Better parsing error messages
- Closely imitate the original behavior wrt immediates and arithmetic
