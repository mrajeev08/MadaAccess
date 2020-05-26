#  Log of major and minor revisions to access draft

May 2020

## Analysis
- Added a section on comparing patient reported travel times to Moramanga ARMC and IPM reported mission driving times to those estimated. Also included comparison of different metrics: distance (km), travel times weighted by population, and unweighted travel times.
- Added a section discussing the bite incidence estimates, and how the estimates between the data sets and scales line up, as well as the outliers
- Removed the analysis attempting to identify and exclude Category I exposures. Primarily because this is incorporated into the range of p_rabid we estimated from Moramanga, and as Fleur pointed out these also reflect how people travel to access care. 
- Changed the range of exposure incidence used, centering around estimates from Moramanga (although constraining the higher end of this to align with what we expect given HDRs & rabies incidence per GAVI paper, explanatory figure added to supplement)
- Used triangular distributions to incorporate uncertainty per previous decision tree studies. 
- Accounted for overdispersion in the data (is data more overdispersed than what you expect given the poisson) & found that when you do this, the random catchment effects are no longer identifiable. So now comparing the district & commune model with overdispersion (generates similar expectations of bite incidence).
- Sampling from the posterior to generate predictions rather than using fixed point estimates
- Added in 2017 vial data
- Changed all figs to include 95% prediction intervals (so the 97.5 and 2.5% quantiles) vs the 95% confidence Intervals around the mean, hence the broader uncertainty. However still taking the quantiles of the national means (i.e. if we expect variation in exposure incidence across the country at any given time point, then what is the mean and quantile distributions sampling from the range of exposure incidence possible)
-  Added a univariate sensitivity analysis to better parse how different parameters contribute to uncertainty. Also increased the upper end of the sensitivity analysis for rabies incidence to be the upper end of exposure incidence estimated from previous Moramanga study.
- Added maps of scaling factors of incidence to show how those sensitvity analyses map on to expectations of underlying rabies incidence. 
- For vial demand predictions, now allocating vial demand by what proportion of the district/commune population is closest to a given clinic (so that a district or commune can be served by multiple clinics)
- Updated the way clinics are added to reflect a more realistic expansion process, i.e. to one per district and then to one per commune. Also compared different ranking metrics against randomly adding clinics to verify that the metric used resulted in the steepest reduction in burden.
- clarified patient reporting vs. undersubmission of forms (per Helene's comments). The undersubmission refers to forms not being submitted by clinics rather than people not seeking PEP.

## Text
- Matched methods and results sections to have the same sub-headings and organization
- Moved all explanatory text from supplement into main manuscript (now supplement just has figures & tables, plus citations of  packages used)
- made a sankey flow diagram to better capture the decision tree (figure 1)
- explained & added in figures showing how clinic throughput shifts as more ARMC are added (lower # of patients presenting per day as catchment size decreases)
- clarified catchment term
- clarified why the effects of adding clinics saturate
- added in  discussion of cost-effectiveness
- added in  discussion of other factors driving access to PEP and clinic functioning (per Claire's MSc) and their importance to improving efficacy of PEP
- added ethics statement, data availability statement, and details on underlying code & software used
- added two 'case studies' on challenges in predicting health seeking behavior from population level predictors

## Note on world pop estimates
- I had issues with the newer estimates of gridded population data from World Pop and from Facebook see [here](https://dataforgood.fb.com/tools/population-density-maps/), so I switched back to the older estimates (which are no longer available through the World Pop website, so may be worth inquiring with them about the discrepancies). These older estimates result in similar estimates of pop size at the regional level given the new census and for the Moramanga District given the recent [census study](https://academic.oup.com/ije/article-abstract/48/6/1754/5606762?redirectedFrom=fulltext)

