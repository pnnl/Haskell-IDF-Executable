# Haskell-IDF-Executable v0.1.0.0

`idf` is a suite of executables for working with EnergyPlus Input Files (IDF).

## Quick Start

* Install the [Haskell Tool Stack](https://haskellstack.org)

* `git clone https://github.com/pnnl/Haskell-IDF-Executable`

* `cd Haskell-IDF-Executable`

* `stack setup && stack build`

[Stackage](https://stackage.org) libraries are installed in `~/.stack`.
Project libraries and extra dependencies are installed in `Haskell-IDF-Executable/.stack-work` (see [here](https://docs.haskellstack.org/en/stable/faq/#where-is-stack-installed-and-will-it-interfere-with-ghc-etc-i-already-have-installed) for more information).

To uninstall `idf`, remove the `Haskell-IDF-Executable` directory, and then remove the `stack` command (see [here](https://docs.haskellstack.org/en/stable/faq/#how-do-i-reset-remove-stack-such-as-to-do-a-completely-fresh-build) for more information).

## Documentation

### `idf-graphviz(0)`

Convert the standard input to a labeled graph in the [DOT language](https://www.graphviz.org/doc/info/lang.html).

#### Synopsis

`idf-graphviz`

#### Description

`idf-graphviz` encodes and displays the labeled graph.
The standard input is decoded using a recursive descent parser for EnergyPlus Input Files (IDF).

Nodes in the graph correspond to classes in the EnergyPlus Data Dictionary (IDD).
Edges in the graph correspond to either hierarchical relationships between classes or references between instances of classes.

#### Options

*No options.*

#### Arguments

*No arguments.*

#### Examples

##### Convert standard input to labeled graph

* `stack exec idf-graphviz < in.idf > out.dot`

* Install [Graphviz](https://www.graphviz.org/)

* `dot -Tpng < out.dot > out.png`

### `idf-pprint(0)`

Pretty print the standard output.

#### Description

`idf-pprint` is a pretty printer.
The standard input is decoded using a recursive descent parser for EnergyPlus Input Files (IDF) and then, immediately,
re-encoded and displayed.

#### Synopsis

`idf-pprint`

#### Options

*No options.*

#### Arguments

*No arguments.*

#### Examples

##### Pretty print standard input

* `stack exec idf-pprint < in.idf > out.idf`

## License

BSD 3-clause "New" or "Revised" license (see the `LICENSE.txt` and `WARRANTY.txt` files in the distribution).

## Contributions

Contributions are accepted on [GitHub](https://github.com/) via the fork and pull request workflow.
See [here](https://help.github.com/articles/using-pull-requests/) for more information.
