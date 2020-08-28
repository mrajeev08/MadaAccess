#  Access to PEP in Madagascar

This repository containes code for the following manuscript:

*Rajeev, M. et al. How access to care shapes disease burden: the current impact of post-exposure prophylaxis and potential for expanded access to prevent human rabies deaths in Madagascar*.

Associated inputs and outputs will be archived via zenodo upon submission to preprint server. Not all data are available due to restrctions on data sharing. In addition some large files are only available on zenodo due to file size limitations.

Code to reproduce the analyses and figures are in [R/](R). The Rmarkdown document and associated supporting files and scripts to reproduce the manuscript are in [docs/](docs).

## Analysis 

1. Process gis data
2. Process bite patient data
3. Run models of bite incidence
4. Simulate clinic expansion 
5. Simulate burden & vials
6. Additional sensitivity analyses
7. Figures 

## Reproducing Results

```
chmod u+x runit.sh

```

## To do

- [ ] Upload inputs & outputs to zenodo (only difference = large files? Or all inputs & outputs this way?)
- [ ] Guide to reproducing results (pull down zenodo & then run master script so that all non-cluster jobs are run)
- [ ] Also list linux dependencies
- [ ] Briefly describe analysis steps & repo structure
- [ ] Briefly describe the data sources


## R Dependencies

| Package      | Version    |
|--------------|------------|
| data.table   | 1.12.4     |
| gdistance    | 1.2-2      |
| Matrix       | 1.2-17     |
| rgdal        | 1.4-6      |
| raster       | 3.0-7      |
| sp           | 1.3-1      |
| malariaAtlas | 0.0.4      |
| ggplot2      | 3.2.1      |
| dplyr        | 0.8.3      |
| doParallel   | 1.0.15     |
| iterators    | 1.0.12     |
| foreach      | 1.4.7      |
| forcats      | 0.4.0      |
| stringr      | 1.4.0      |
| purrr        | 0.3.2      |
| readr        | 1.3.1      |
| tidyr        | 1.0.0      |
| tibble       | 2.1.3      |
| tidyverse    | 1.2.1      |
| glue         | 1.3.1      |
| triangle     | 0.12       |
| doRNG        | 1.7.1      |
| rngtools     | 1.4        |
| pkgmaker     | 0.27       |
| registry     | 0.5-1      |
| magrittr     | 1.5        |
| cowplot      | 1.0.0      |
| patchwork    | 1.0.0.9000 |
| lme4         | 1.1-21     |
| stringdist   | 0.9.5.2    |
| lubridate    | 1.7.4      |
| rgeos        | 0.5-2      |
| rmapshaper   | 0.4.3      |
| rjags        | 4.10       |
| coda         | 0.19-3     |
