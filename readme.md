#  Access to PEP in Madagascar
[![DOI](https://zenodo.org/badge/149655745.svg)](https://zenodo.org/badge/latestdoi/149655745)

This repository accompanies the manuscript:

*Rajeev, M. et al. How access to care shapes disease burden: the current impact of post-exposure prophylaxis and potential for expanded access to prevent human rabies deaths in Madagascar*. [add medrxiv link once up]

Code to reproduce the analyses and figures are in [analysis/src](analysis/src). The Rmarkdown document and associated supporting files to reproduce the manuscript are in [analysis/paper](analysis/paper).

## Reproducing results

All analyses were completed in R version 4.0.2. All rmarkdown outputs require `rmarkdown`.

### Step 1: Clone the repository & also download intermediate outputs from zenodo. 

```
git clone https://github.com/mrajeev08/MadaAccess.git
```

Also, download the outputs from [this zenodo link](https://doi.org/10.5281/zenodo.4064312)(also includes some of the raw data used in the analysis, although not all due to sharing restrictions). 


### Step 2: Install dependencies
- Option 1: using `renv` (will install version specific to renv & local package library)
In R or Rstudio run this command:
```
renv::restore()
```
This will throw a warning if you're using a different R version from 4.0.2 used for this analysis. 

- Option 2: Source the script [analysis/src/00_install.R](analysis/src/00_install.R) (will install most recent to global package library).

**You will also need to download and install [JAGS](https://sourceforge.net/projects/mcmc-jags/files/).** 
  
### Step 3: Run the analysis
The scripts are numbered sequentially according to how the should be run. 

If you're in a unix environment, you can use the bash script runit.sh to run all analysis steps sequentially by:

```
./runit.sh -d "analysis/src/*/*" --printErrors -q
```

or to just make the figures:
```
./runit.sh -d "analysis/src/figures/*" --printErrors -q
```

or to make the paper (word doc outputs):
```
./runit.sh -d "analysis/src/make_paper.R" --printErrors -q
```

To run the steps which were completed on a cluster, you will need to pass the `-cl` argument:
```
./runit.sh -d "analysis/src/*/*" --printErrors -q -cl
```
and then enter 1 to run the scripts in parallel or 2 to run serially on your local computer. 

Some of these analysis steps require significant compute time & RAM, see the requirements [here](logs/log_last_ran.csv). If you do have ssh access to a remote cluster with a slurm scheduler and want to run jobs, I wrote this() shell utility to run the cluster jobs which you could adapt accordingly.

To see the help for `runit.sh`, run `./runit.sh -h` on the command line. 

Note that for the shell scripts, you may need to make them executable, i.e.:
```
chmod u+x runit.sh
```

## Acknowledgements & hat-tips

- Figure inspiration: 
  - Fig 3 inspiration from [Gytis Dudas](https://evogytis.github.io/posts/2019/03/animation/)
  - Fig 5 inspiration from [Maarten Lambrechts](https://www.maartenlambrechts.com/2019/09/04/splitting-EU-regions-making-of.html)
  - Geom raincould gist from [David Robinson](https://gist.github.com/dgrtwo/eb7750e74997891d7c20)

- Resources on building a research compendium: 
  - [rrtools](https://github.com/benmarwick/rrtools)
  - [workflowr](https://github.com/jdblischak/workflowr)
  - [renv](https://rstudio.github.io/renv/articles/renv.html)

## To do
- [ ] Docker/binder
- [ ] Pkgdown site
