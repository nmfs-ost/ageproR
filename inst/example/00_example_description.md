ageproR Projection Samples
=====================================================================

The following example comes from _Stock Assessment of Uku (Aprion virescens) in Hawaii, 2020_ (Nadon et al. 2020). This example is provided to illustrate projection options and features of this R package. These projections use fishery data but are for the purposes for demonstration only.

### Hawaii Uku Example
This example is a fishing mortality and stock biomass projection for the uku snapper in Hawaii. The projection was conducted using results from the base-case Stock Synthesis model to evaluate the probable impacts of constant catch quotas on future spawning stock biomass and yield with a time horizon of 2019-2026. This projection includes four fishing fleets with distinct landings quotas, mean weights at age, and fishery selectivities at age as well as using three recruitment models with different probabilities of being the future state of nature. The initial condition for the stochastic projection was based on the distribution of estimated initial population size-at-age in the year 2018. A total of 1000 simulations were run for each of 100 bootstrap replicates to characterize the effects of uncertainty in initial stock size as well as process errors on the distribution of future recruitment, life history, and fishery parameters.
