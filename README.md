# Unorthodox-Expert-System
An Unorthodox Expert System based on a different interpretation of the Karnaugh Map.

See the index.html file for general ideas, the code does not follow it exactly.

Run under Rust, a command help screen will display.  

Simply pressing the Enter key and seeing what happens is all you need to do.

The program is an expert of its own state, a series of bits.  Various (canned) actions change the state and the program
gets a progressively better understanding of what the actions do.  The program seeks samples to 
improve its understanding.

The goal is a better and better understanding of the effects of the actions, due to samples gathered by circumstance and
intention. Then be able to plan, and execute, a series of actions to change the current state to a goal state.

A final, exact, understanding is NOT the goal, just as there is a lot that I do not understand, but
I can still muddle through life. It is also NOT the goal to take every possible sample, if you had a
64-bit state, it would not be practical to take every sample.
