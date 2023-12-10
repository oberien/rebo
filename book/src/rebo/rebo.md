# rebo

Rebo is an expression-based scripting language with syntax heavily inspired by rust.
It is statically typed and has backwards type inference.
When executing a script, rebo fully analyses and checks the source code
including all types, before it runs.
While that approach is usually used for AOT (Ahead Of Time compilers),
the execution of rebo is interpreted and thus embeddable on all systems.
This approach also allows defining types and functions after their first use.
