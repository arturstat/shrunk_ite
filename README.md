# shrunk_ite
Data simulation from: Shrunk versus naive individual treatment effects in series of n-of-1 trials

## Funding
This work was supported by the European Union’s FP7 programme [grant number 602552] for the IDEAL project.

## include/.

The files required to perform the simulation are placed inside this folder.

## Figure1.R

Creates a plot of the Mean Squared Error versus the true individual treatment effect. Two curves are drawn, one curve for the shrunk estimate, and another curve for the naive estimate of the individual treatment effect. A random intercepts model is assumed for the population of true individual treatment effects. The curve relative to the shrunk estimate assumes a theoretical series of n-of-1 trials with an unlimited number of subjects. The probability density function of the true individual treatment effect is added over the plot, alongside a second y-axis.

## Figure2.R

Creates a plot of the Mean Squared Error versus the estimated individual treatment effect. Two curves are drawn, one curve for the shrunk estimate, and another curve for the naive estimate of the individual treatment effect. A random intercepts model is assumed for the population of true individual treatment effects. The curve relative to the shrunk estimate assumes a theoretical series of n-of-1 trials with an unlimited number of subjects. The probability density functions of the shrunk and naive estimates are added over the plot, alongside a second y-axis.

## Figure3.R

Simulates series of n-of-1 trials with: 1, 2, 3, 4, 5, 10, 20, 100, 1000 subjects. A random intercepts model is assumed for the population of true individual treatment effects. The squared differences between the true individual treatment effect, and the shrunk estimate of the individual treatment effect, are computed for each subject. Then, the squared differences are regressed on the shrunk estimate, using local polynomial regression. This approach leads to a distribution-free estimate of the Mean Squared Error conditioned on the shrunk estimate. Finally, a plot of the Mean Squared Error versus the shrunk estimate is created. Nine curves are drawn, one curve for each number of subjects mentioned above. Simulation and estimation are computed using all the parallel threads available in the local machine. Despite the use of parallel computation, due to the high number of simulations, the whole program may take several hours to complete. Around 3.45GB of data are generated.

## Figure4.R

Simulates series of n-of-1 trials with a number of subjects ranging from 2 to 1000. A random intercepts model is assumed for the population of true individual treatment effects. The squared differences between the true individual treatment effect, and the shrunk estimate of the individual treatment effect, are computed for each subject. Then, the squared differences are averaged over both: the computed shrunk estimates, and the simulated individual treatment effects. The obtained Mean Squared Error is divided by the theoretical Mean Squared Error associated to the naive estimate, yielding the relative efficiency between shrunk and naive estimates. Finally, the relative efficiency is plotted against the number of subjects for the defined covariance parameters. All the parallel threads available in the local machine are used for computation. Despite the use of parallel computation, due to the high number of computations, the whole program may take several hours to complete.

