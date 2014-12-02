# Robots

This program is a distributed robot mining simulator, written in Erlang. It takes
place in the curriculum of the Distributed Algorithm course of the ENS Lyon.

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

The terminal now displays the evolution of the map.

Quit the program:

    ctrl+C

Some results (points scored, eventual runtime errors) are written in the file `main.log`.

#### Interactive mode

This mode offers a user interface, which allow to query information about the system
state, and to terminate properly the program.

Run the program:

In a first terminal:

    make interactive

In a second terminal:

    make ui

The first terminal now displays the evolution of the map. The second terminal is an
Erlang shell offering some functions:

* `ui:allNames()`   will display the name of all robots of the map.
* `ui:printScore()` will display the different points scored by the robots till now.
* `ui:terminate()`  will terminate properly the program.

Some results (points scored, queries from the UI, eventual runtime errors) are written in the file `main.log`.

#### Superarbiter

The program is intended to run with some provided superarbiter. It should work, but
has not been tested. However, one can run the program with some dummy superarbiter:
in the file `main.erl`, uncomment the line `register(superarbiter,spawn(main,superarbiter_loop,[])),`,
then compile and run.


## Distributed algorithms

We describe here the strategy used to efficiently find gold.

Each time a robot has no available message, it scans its neighbour cells by sending
info queries to the arbiter. This is more efficient than walking/mining a cell without
knowledge, since an info query takes no time whereas an action query takes more than 250ms.

When a robot detect gold, it collect it.

Once the scan is terminated (and all the gold of the neighbour cells has been collected),
the robot move to a free neighbour cell, and start this process again.

The fundamental decision is the choice of such a free cell to move on.
The robot will chose randomly among the cells where no robot have never been. If
no such cell exist, it will chose randomly among all the free cells. If there is
no free cell at all, it stays in place (and will restart the process in a few time).

Once the robot chose a cell, it performs a move query to the arbiter, and informs
the other robots of this new action. By doing so, all robots know in real-time what
cells have been visited.
