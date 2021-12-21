# Unorthodox-Expert-System
An Unorthodox Expert System, an expert of its own state.

Based on an improved interpretation of the Karnaugh Map.  Karnaugh Maps have a bug, and there is a fix, see the top of the file theory.html

There is extensive use of Boolean Algebra, which is supported by almost any programming language.

Run under Rust, a command help screen will display.  

Simply pressing the Enter key and seeing what happens is all you need to do.

The program state is a series of bits.  Various (canned) actions change the state and the program
gets a progressively better understanding of what the actions do.  The program seeks samples to 
improve its understanding.  No human intervention is required.

The goal is a better and better understanding of the effects of the actions, due to samples gathered by circumstance and
intention. Then be able to plan, and execute, a series of actions to change the current state to a goal region.  The command
"to r010X" will attempt to change the current state to 0101 or 0100.

A final, exact, understanding is NOT the goal, just as there is a lot that I do not understand, but
I can still muddle through life. It is also NOT the goal to take every possible sample, if you had a
64-bit state, it would not be practical to take every sample.

Assume a device with a state made of a number of bits, actions that can change the bits, and a definition of a number of states that qualify as optimal.
After some testing of actions, the device can develop rules which can then be used to put the device into an optimal state.
If something happens to change the state of the device to a non-optimal state, the device can again run rules to put it into an optimal state.
If a rule does not work as expected, it can be deleted and new rules can be developed.

At a higher level, maybe something gets "bored", so the optimum state changes.  At some level are we following something like: food -> shelter -> rest -> food ?
