# Unorthodox-Expert-System
An Unorthodox Expert System, an expert of its own state, rules can be developed and used without human intervention.

A number of important processes can be run in parallel, without being slowed down by needing to change shared memory.

There is extensive use of Boolean Algebra.

Based on an improved interpretation of the Karnaugh Map.  Karnaugh Maps have a bug, and there is a fix, see the top of the file theory.html

Run under Rust, a command help screen will display.  

Simply pressing the Enter key and seeing what happens is all you need to do.

To start, the current state is not predictable, so a need is created for all possible actions, to get a sample of the state.

As samples are taken, and the current state changes for some samples, the program combines similar samples/results into regions, called "groups", which predict sample results for states that may not have been sampled yet.

After 50 to 60 samples of the current state (you can lean on the Enter key), enough groups are formed to allow the program to plan actions to change the current state as needed.

What is needed?  Resampling a state to insure that the change it causes is repeatable.  Sampling a state to enlarge an existing group.  Sampling a state where two groups intersect and make different predictions.  Sampling states adjacent to a group to test the boundaries of the group.

Plans appear as something like "P[1,2,4]" which means: Run action 1, then 2, then 4, to get to a desired state.  Then run the need action to get the desired sample.  The command "ppd need-number" shows a good map of the plan.

After a number of groups are formed, the user can change the state with a command like "to r010X", which will attempt to change the current state to 0101 or 0100.

A final, exact, understanding is NOT the goal, just as there is a lot that I do not understand, but
I can still muddle through life. It is also NOT the goal to take every possible sample, if you had a
64-bit state, it would not be practical to take every sample.

Assume a device with a state made of a number of bits, actions that can change the bits, and a definition of a number of states that qualify as optimal.
After some testing of actions, the device can develop rules which can then be used to put the device into an optimal state.
If something happens to change the state of the device to a non-optimal state, the device can again run rules to put it into an optimal state.
If a rule does not work as expected, it can be deleted and new rules can be developed.

At a higher level, maybe something gets "bored", so the optimum state changes.  At some level are we following something like: food -> shelter -> rest -> food ?
