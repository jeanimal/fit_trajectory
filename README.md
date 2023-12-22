fit_trajectory
==============

Fit trajectories show how, and how fast, sampling variance across samples shrinks as sample size increases for a given estimation method.

How it works: Each point represents the fitted parameter values from one sample of data; then fit a trajectory connecting points represents how the fit "moves" as more data is added to that sample.  The movement is messy due to sampling variance, but as sample size increases, the fit trajectories from  various samples gradually converge to the mean fitted point for that estimation method.  After convergence, error is only from the gap ("bias") from the mean fitted parameter vlaues to the "true" parameter values.

Note this views an estimator from parameter space rather than data space.

See rendered introduction: https://htmlpreview.github.io/?https://github.com/jeanimal/fit_trajectory/blob/master/fit_trajectory_intro.html
