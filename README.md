filter-agda-dependency-graph
============================

This tool filters the output of `agda --dependency-graph` by

  * removing library modules
  * removing direct dependencies where there's also an indirect
    dependency

Usage
=====

The tool assumes that you start it in the base directory of your
Agda codebase.

 1. Create a dependency graph.

        agda -i . -i /some/library/src -i /another/library/src MainModule.agda --dependency-graph big.dot

    The `big.dot` graph will contain modules from all three
    search path components.

 2. Filter the dependency graph.

        filter-agda-dependency-graph < big.dot > small.dot

    The `small.dot` will only contain modules from `.` but not
    from `/some/library/src` or `/another/library/src`.

 3. Layout the graph.

        dot -Tpdf small.dot > small.pdf
