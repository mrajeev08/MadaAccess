####################################################################################################
##' Agreggating and processing data to input into bite incidence models
##' Details: 
##' Author: Malavika Rajeev 
####################################################################################################

## Libraries
library(tidyverse)

## Steps for processing data

##' 1. Filter out transfers (these are people that came from other clinics)
##' 2. Filter Cat 1 based on clinic throughput (compare to doses delivered to clinic in metadata)
##' 3. Calc reporting
##' 4. Summarize by year and admin unit
##' 5. Filter out excluded catchments and correct for reporting
