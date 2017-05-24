# vicarp: Victimisation Analysis and Reporting

A convienience package to facilitate analysis and reporting of 
victimisation data analysis. Though developed for this purpose,  
functions can be applied to different fields where the analytical needs 
are similar (e.g., studying prevalence, count data distributions).

## To do:

- Add corstars function
- Add grouped lm function
- ~Add auto table distribution function~ (~Still need to add cumulative percentages, generate lorenz curves~)
- ~Add mc_gini_test function~ (~Test batch mode~, generate a grid of plots: More complicated than I thought, not really worth it?)
- ~Add index of dispersion test~ (~add batch helper function~)
- ~Add a test for an observed vs expected KS-Test for a count DV (Use the MC_gini_test Replicates)~ Do generate Obs-exp Poisson and NB tables with hypothesis testing
- Add model deviance function (for glmmADMB objects?)
- ~Add a function for a table of contingency tables, including percentages
  and ratio of observed/expected frequencies~
- Add function to calculate ICC of a three level model.
