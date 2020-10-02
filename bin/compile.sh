#!/bin/bash

# Cleaning out everything
./clean -all -dataraw -della

# Step 1: process raw data
./runit.sh -d "data-raw/src/*" -cl -q -printErrors

# Step 2: run all analyses (will run each step w/in sub folders)
./runit.sh -d "analysis/src/*/*" -cl -q -printErrors

# Step 3: compile manuscript files

# Step 4: document functions

# Step 5: build pkgdown website

# Push to github

# Zenodo tarball (docker & out)


