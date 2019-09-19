####################################################################################################
##' Agreggating and processing data to input into bite incidence models
##' Details: 
##' Author: Malavika Rajeev 
####################################################################################################


## Stats on transfers
## Date limits and reporting
## Contacts
## Bite incidence estimates


##' 1. Filter out transfers (these are people that came from other clinics)
##' 2. Filter Cat 1 based on clinic throughput (compare to doses delivered to clinic in metadata)
##' 3. Calc reporting
##' 4. Summarize by year and admin unit
##' 5. Filter out excluded catchments and correct for reporting

##' 6. Make figure of bite incidence for Mada and Mora