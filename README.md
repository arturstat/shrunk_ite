# shrunk_ite
Data from: Shrunk versus naive individual treatment effects in series of n-of-1 trials

include./

The files required to perform the simulation are placed inside this folder.

Figure1.R

Creates a plot of the Mean Squared Error versus the true individual treatment effect. Two curves are drawn, one curve for the shrunk estimate, and another curve for the naive estimate of the individual treatment effect. A random intercepts model is assumed for the population of true individual treatment effects. The curve relative to the shrunk estimate assumes a theoretical series of n-of-1 trials with an unlimited number of subjects.

Figure2.R

Creates a plot of the Mean Squared Error versus the estimated individual treatment effect. Two curves are drawn, one curve for the shrunk estimate, and another curve for the naive estimate of the individual treatment effect. A random intercepts model is assumed for the population of true individual treatment effects. The curve relative to the shrunk estimate assumes a theoretical series of n-of-1 trials with an unlimited number of subjects.

Figure3.R

Simulates series of n-of-1 trials with: 1, 2, 3, 4, 5, 10, 20, 100, 1000 subjects. A random intercepts model is assumed for the population of true individual treatment effects. The squared differences between the true individual treatment effect, and the shrunk estimate of the individual treatment effect, are computed for each subject. Then, the squared differences are regressed on the shrunk estimate, using local polynomial regression. This approach leads to a distribution-free estimate of the Mean Squared Error conditioned on the shrunk estimate. Finally, a plot of the Mean Squared Error versus the shrunk estimate is created. Nine curves are drawn, one curve for each number of subjects mentioned above. Simulation and estimation is computed using all the parallel threads avaialble in the local machine. Despite the use of parallel computation, due to the high number of simulations, the whole computation may take several hours to complete. Around 3.45GB of data are generated.
