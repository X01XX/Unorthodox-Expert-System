# Unorthodox-Expert-System
An expert of its own state. Rules can be developed, revised, and used without human intervention. The rules are low-level, probably not what you expect.

Rule characteristics allow it to be known that some rules apply to a desired change, and others do not.  Some rule pairs can be known to need to be used in a given order, or even be mutually exclusive.  A single available rule that makes part of a desired change, means the problem must be split into going from the current state to a state where the rule can be applied, then from the result of the rule to the goal state.

Based on an improved interpretation of the Karnaugh Map.  Karnaugh Maps have a bug, and there is a fix, see the top of the file theory.html. Also, squares that are only in one region have a special significance.

A number of important processes can be run in parallel, without being slowed down by needing to change shared memory.

There is extensive use of Boolean Operations.

Run under Rust, a command help screen will display.  

Simply pressing the Enter key and seeing what happens is all you need to do.

To start, the current state is not predictable, so a need is created for all actions, to get a sample of the state.

As samples are taken, and the current state changes for some samples, the program combines similar samples/results into regions, called "groups", which predict sample results for states that may not have been sampled yet.

After 50 to 60 samples of the current state (you can lean on the Enter key), enough groups are formed to allow the program to plan actions to change the current state as needed.

What is needed?  Sampling a state to insure that the change an action causes is repeatable.  Sampling a state to enlarge an existing group.  Sampling a state where two groups intersect and make different predictions.  Sampling states adjacent to a square that is only in one group, to test the boundaries of the group.

Plans appear as something like "P[1,2,4]" which means: Run action 1, then 2, then 4, to get to a desired state.  Then run the need action to get the desired sample.  The command "ppd need-number" shows a good map of the plan.

If a plan does not work as expected, rules are updated, then an attempt is made to recalculate, and execute, a plan to the goal.

After a number of groups are formed, the user can change the state with a command like "to r010X", which will attempt to change the current state to 0101 or 0100.

A final, exact, understanding is NOT the goal, just as there is a lot that I do not understand, but
I can still muddle through life. It is also NOT the goal to take every possible sample, if you had a
64-bit state, it would not be practical to take every sample.

Assume a device with a state made of a number of bits, actions that can change the bits, and a definition of a number of states that qualify as optimal.
After some testing of actions, the device can develop rules which can then be used to put the device into an optimal state.
If something happens to change the state of the device to a non-optimal state, the device can again run rules to put it into an optimal state.

At a higher level, maybe something gets "bored", or "satiated", so the optimum state changes.  At some level are we following something like: food -> shelter -> rest -> food ?

Regions that are considered negative can be added.  When making plans, the program will try not to traverse negative regions.

A "domain" is a state and a set of actions.  There can be more than one domain.  Each domain state can have an arbitrary number of bits. A goal includes all domains, plans to achieve the goal will be made for each domain where its state is not within the domain goal.

There is a command for storing the states, and rules, to a text file.  Running the program with the file path as an argument loads the file.
