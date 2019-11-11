# shrunk_ite
Data from: Shrunk versus naive individual treatment effects in series of n-of-1 trials

include./

The code files required to perform the simulation are placed inside this folder.

Figure1.R

Creates a plot of the Mean Squared Error versus the true individual treatment effect. Two curves are drawn, one curve for the shrunk estimate, and another curve for the naive estimate of the individual treatment effect. A random intercepts model is assumed for the population of true individual treatment effects.

Figure2.R

Creates a plot of the Mean Squared Error versus the estimated individual treatment effect. Two curves are drawn, one curve for the shrunk estimate, and another curve for the naive estimate of the individual treatment effect. A random intercepts model is assumed for the population of true individual treatment effects.

Figure3.R

Simulates series of n-of-1 trials with 1, 2, 3, 4, 5, 10, 20, 100, 1000 subjects. A random intercepts model is assumed for the population of true indivudal treatment effects.
