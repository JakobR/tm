# tm

A Turing Machine Simulator.


## How to build

* Install the Haskell build tool `stack` from https://docs.haskellstack.org/en/stable/README/

* Run `stack build` to build the program. This should also download the compiler and all dependencies.

* Run the compiled program with `stack exec tm`.


## Test new Turing machines

This is specific to the assignment I'm currently checking.

1. Add definition of new Turing machine to module `TuringMachine.Examples`.

2. Change variable `tmForTesting` to the new definition.

3. Rebuild with the command `stack build`.

4. Run with `stack exec tm`.

Note: before trusting the result, you have to make sure that the definition of the TM is correct!  Many failures I've encountered were actually due to mistakes in the transcription of the TM rather than a wrong solution.
