# Robots

This program is a distributed robot mining simulator, writen in Erlang. It takes
place in the curiculum of the Distributed Algorithm course of the ENS Lyon.

Several robots are initialized on some map, containing free cells and blocks. Free
cells may contain gold. Robots aim to collect gold as fast as possible, by performing
a collective search.

## Get started

Compile the program:

    make

Run the unitary tests (consider using smaller constants `TIME_ENTER`, `TIME_MOVE`
and `TIME_COLLECT` in the file `myArbiter.erl`, to decrease the test running time):

    make test

The program has two modes: a standalone and an interactive mode.

#### Standalone mode

This mode does not allow any action of the user. Just launch it, and watch.

Run the program:

    make standalone

The terminal now display the evolution of the map.

Quit the program:

    ctrl+C

Observe results:

    cat main.log

#### Interractive mode

This mode offer a user interface, which allow to query informations about the system
state, and to terminate properly the program.

Run the program:

In a first terminal:

    make interactive

In a second terminal:

    make ui

The first terminal now display the evolution of the map. The second terminal is an
erlang shell offering some functions:

* `ui:allNames()`   will display the name of all robots of the map.
* `ui:printScore()` will display the different points scored by the robots till now.
* `ui:terminate()`  will terminate properly the program.

Observe results:

    cat main.log
