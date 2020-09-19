# 1. Get travel times -----------------------------------------------------------------------------
#' \code{get_ttimes} calculates the minimum travel times/distance for an input raster to
#' an input set of
#' GPS points.
#' This function uses the friction surface from the Malaria Atlas Project. Script adapted
#' from https://map.ox.ac.uk/research-project/ttimesibility_to_cities/. Uses least-cost algorithms
#' from the gdistance package.
#' @param friction raster, the friction surface downloaded from MAP website
#' @param shapefile polygon shapefile, to mask the friction surface to
#' @param coords matrix of two columns of x (first col, longitude) and y (second col, latitude) points to
#'   input to calculate the least-cost distance (here travel times)
#' @param trans_matrix_exists logical, if TRUE then looks for file as specified by filename_trans
#'   if FALSE then creates the transition matrix using function transition from gdistance package
#' @param filename_trans character, the path to which the transition matrix should either be
#'   read from or written to
#' @return raster at same resolution as the input friction surface and cropped to the shapefile with
#'   the minimum ttimes metric estimate as the values
#' @section Dependencies:
#'  Packages: gdistance, raster, sf, sp

get_ttimes <- function(friction, shapefile, coords, trans_matrix_exists = TRUE,
                       filename_trans) {

  # crop friction surface to shapefile
  friction <- crop(friction, shapefile)

  # Fetch the number of points
  n_points <- nrow(coords)

  # Make the graph and the geocorrected version of the graph (or read in the latter).
  if (trans_matrix_exists == TRUE) {
    # Read in the transition matrix object if it has been pre-computed
    trans_gc <- readRDS(filename_trans)
  } else {
    # Make and geocorrect the transition matrix (i.e., the graph)
    trans <- transition(friction, function(x) 1 / mean(x), 8) # RAM intensive,
    # can be very slow for large areas
    trans_gc <- geoCorrection(trans)
    saveRDS(trans_gc, filename_trans)
  }

  # Run the accumulated cost algorithm to make the final output map.
  # This can be quite slow depending on the area
  ttimes <- accCost(trans_gc, coords)

  # Return the resulting raster
  return(ttimes)
}

# 2. Getting ranked clinics and district/commune covariates  ----------------------------------
#' Rank clinics and summarize travel times and catchments at admin level
#' \code{add_armc} gets ranked ARMC and associated travel times and catchments at the district and
#' commune levels using catchment matrix of travel times at each grid cell (rows) for each clinic
#' (columns) to rank which order clinics should be added based on how adding them shifts the
#' distribution of travel times at the population level.
#'
#' @param base_df a data.table with the following rows for each grid cell:
#'  ttimes (the baseline travel times), prop_pop (the proportion of the population),
#'  catchment (the baseline clinic catchment id), pop_dist (the total population in the district
#'  in which the grid cell falls), pop_comm (the total population in the commune in which the grid
#'  cell falls), commcode (corresponds to commcode in shapefile)),
#'  distcode (corresponds to distcode in shapefile);
#' @param clinic_names vector of the names or ids of the candidate ARMC to be added
#' @param clinic_catchmat a matrix of ttimes estimates for each of the grid cells (rows) in the
#' shapefile for each of the candidate clinics (columns, should match length of clinic_names vector)
#' @param prop_pop a numeric vector of the proportion of the total population in each grid cell
#' @param max_clinics numeric, the number of clinics that you want to add (when to stop adding)
#' @param thresh_ttimes numeric, the threshold travel times, any decreases in travel times resulting
#' from addition of a clinic are ignored (trying to target populations with worst access)
#' @param thresh_prop numeric between 0-1, if a clinic is added and it shifts travel times above
#' thresh_ttimes but only for less than thresh_prop, then the clinic is filtered out
#' @param dir_name the directory name to output the resulting data frames into
#' @param overwrite boolean, if TRUE then overwrites data at first step and then appends; if FALSE
#' then appends all to existing files
#'
#' @return the final travel times and catchments at the grid cell level
#' @details The results, aggregated to the district and commune levels, are written to a file at each
#' step to limit memory used. Note that the pop_wt_comm/dist is the population for which travel times are
#' not infinite for a given clinic
#' @section Dependencies:
#'     Packages: data.table, foreach

# Pass through base df, otherwise you need all the vectors!
add_armc <- function(base_df, cand_df, max_clinics, thresh_ttimes, dir_name, rank_metric,
                     base_scenario = 0, thresh_met = 0, overwrite = TRUE, random = TRUE) {

  base <- copy(base_df) # so as not to modify global env
  cand <- copy(cand_df) # so as not to modify global env

  setDTthreads(1)

  # Pull in references to files (that way not as big!)
  cand_list <- vector("list", nrow(cand))
  for(j in 1:nrow(cand)) {
    cand_list[[j]] <- raster(cand$candfile[j])
  }

  # Make the directory if it doesn't exist
  if (!dir.exists(dir_name)) dir.create(dir_name, recursive = TRUE)

  for (i in 1:max_clinics) {
    if (random == TRUE) {

      # Randomly add clinics
      ranks <- sample(1:length(cand_list), length(cand_list), replace = FALSE)
    } else {

      # Dummy sum prop to be replaced inside loop
      ranks <- 0

      while (sum(ranks, na.rm = TRUE) == 0 & length(cand_list) > 1) {
        foreach(j = iter(cand_list), .combine = c, .packages = "raster") %dopar% {
          rank_metric(base, cand_ttimes = j[], thresh_ttimes)
        } -> ranks

        # If no candidates that would shift travel times above the threshold
        # Then adjust threshold down
        if (sum(ranks, na.rm = TRUE) == 0 | length(ranks[ranks >= thresh_met]) == 0) {
          thresh_ttimes <- 0.75 * thresh_ttimes
        }
      }
    }

    clin_chosen <- cand[which.max(ranks), ]
    metric_val <- max(ranks[!is.infinite(ranks)], na.rm = TRUE)
    rank_df <- data.table(scenario = i + base_scenario, clin_chosen, metric_val)
    new_ttimes <- cand_list[[which.max(ranks)]][] # add to base

    # Take out the max one
    cand <- cand[-which.max(ranks), ]
    cand_list <- cand_list[-which.max(ranks)]

    # Aggregating to district & commune
    # If ttimes improved then, new ttimes replaces the baseline and catchment

    base[, c("ttimes", "catchment") :=
      .(
        fifelse((new_ttimes < ttimes & !is.na(new_ttimes)) |
          (!is.na(new_ttimes) & is.na(ttimes)), new_ttimes, ttimes),
        fifelse((new_ttimes < ttimes & !is.na(new_ttimes)) |
          (!is.na(new_ttimes) & is.na(ttimes)), clin_chosen$clinic_id, catchment)
      )]

    district_df <- aggregate_admin(base_df = base, admin = "distcode", scenario = i + base_scenario)
    district_maxcatch <- district_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)],
      by = .(distcode, scenario)
    ]

    # Commune
    commune_df <- aggregate_admin(base_df = base, admin = "commcode", scenario = i + base_scenario)
    commune_maxcatch <- commune_df[, .SD[prop_pop_catch == max(prop_pop_catch, na.rm = TRUE)],
      by = .(commcode, scenario)
    ]

    # Do the max one here too!
    if (overwrite == TRUE & i == 1) { # overwrite on first one & use gzip to commpress)
      fwrite(district_df, paste0(dir_name, "/district_allcatch.gz"))
      fwrite(commune_df, paste0(dir_name, "/commune_allcatch.gz"))
      fwrite(district_maxcatch, paste0(dir_name, "/district_maxcatch.gz"))
      fwrite(commune_maxcatch, paste0(dir_name, "/commune_maxcatch.gz"))
      fwrite(rank_df, paste0(dir_name, "/clinics_ranked.csv"))
    } else {
      fwrite(district_df, paste0(dir_name, "/district_allcatch.gz"), append = TRUE)
      fwrite(commune_df, paste0(dir_name, "/commune_allcatch.gz"), append = TRUE)
      fwrite(district_maxcatch, paste0(dir_name, "/district_maxcatch.gz"), append = TRUE)
      fwrite(commune_maxcatch, paste0(dir_name, "/commune_maxcatch.gz"), append = TRUE)
      fwrite(rank_df, paste0(dir_name, "/clinics_ranked.csv"), append = TRUE)
    }

    print(paste(
      i, "clinic added:", clin_chosen$district, "District", clin_chosen$commune,
      "Commune"
    ))
  }
  return(base_df) # return updated baseline
}

# Get grid cell catchments for set of clinics ------------------------------------------------
update_base <- function(cand_df, base_df, nsplit = 2) {
  setDTthreads(1)

  base <- copy(base_df)

  # Pull in references to files (that way not as big!)
  cand_list <- vector("list", nrow(cand_df))

  foreach(j = 1:nrow(cand_df), .packages = "raster") %do% {
    cand_list[[j]] <- raster(cand_df$candfile[j])
  }

  names(cand_list) <- cand_df$clinic_id

  # Split candidates
  out <- split(cand_list, rep(1:nsplit, each = (ceiling(length(cand_list) / nsplit))))

  multicomb <- function(x, ...) {
    mapply(cbind, x, ..., SIMPLIFY = FALSE)
  }

  foreach(
    i = 1:length(out),
    .packages = c("raster", "data.table"), .combine = multicomb
  ) %dopar% {
    sub <- out[[i]]
    cand_ttimes <- sub[[1]][]
    cand_catch <- ifelse(!is.na(cand_ttimes), names(sub)[1], NA)

    for (j in 2:length(sub)) {
      cand_comp <- sub[[j]][]
      cand_catch <- fifelse(
        (cand_comp < cand_ttimes & !is.na(cand_comp)) |
          (!is.na(cand_comp) & is.na(cand_ttimes)), names(sub)[j],
        cand_catch
      )
      cand_ttimes <- fifelse((cand_comp < cand_ttimes & !is.na(cand_comp)) |
        (!is.na(cand_comp) & is.na(cand_ttimes)), cand_comp, cand_ttimes)
    }
    list(catches = cand_catch, ttimes = cand_ttimes)
  } -> catches

  min_ttimes <- catches[["ttimes"]]
  min_catches <- catches[["catches"]]
  cand_ttimes <- min_ttimes[, 1]
  cand_catch <- min_catches[, 1]

  for (i in 2:ncol(min_ttimes)) {
    cand_comp <- min_ttimes[, i]
    cand_catch_comp <- min_catches[, i]
    cand_catch <- fifelse((cand_comp < cand_ttimes & !is.na(cand_comp)) |
      (!is.na(cand_comp) & is.na(cand_ttimes)), cand_catch_comp, cand_catch)
    cand_ttimes <- fifelse((cand_comp < cand_ttimes & !is.na(cand_comp)) |
      (!is.na(cand_comp) & is.na(cand_ttimes)), cand_comp, cand_ttimes)
  }

  cand_catch <- as.numeric(cand_catch)

  base[, c("ttimes", "catchment") :=
    .(
      fifelse((cand_ttimes < ttimes & !is.na(cand_ttimes)) |
        (!is.na(cand_ttimes) & is.na(ttimes)), cand_ttimes, ttimes),
      fifelse((cand_ttimes < ttimes & !is.na(cand_ttimes)) |
        (!is.na(cand_ttimes) & is.na(ttimes)), cand_catch, catchment)
    )]

  return(base)
}


#' Process outputs of ttimes
#' Description
#' Details
#' @param Paramters
#' @return Returned
#' @section Dependencies:
#'     List dependencies here, i.e. packages and other functions

process_ttimes <- function(dir_name = "analysis/out/ttimes/addclinics",
                           include_base = TRUE,
                           base_dir = "analysis/out/ttimes/base") {

  files <- list.files(dir_name, full.names = TRUE)

  comm_all <- rbindlist(lapply(grep("commune_allcatch", files, value = TRUE), fread), fill = TRUE)
  comm_max <- rbindlist(lapply(grep("commune_maxcatch", files, value = TRUE), fread), fill = TRUE)
  dist_max <- rbindlist(lapply(grep("district_maxcatch", files, value = TRUE), fread), fill = TRUE)

  if (include_base == TRUE) {
    comm_all <- rbind(comm_all, fread(paste0(base_dir, "/", "commune_allcatch.gz")), fill = TRUE)
    comm_max <- rbind(comm_max, fread(paste0(base_dir, "/", "commune_maxcatch.gz")), fill = TRUE)
    dist_max <- rbind(dist_max, fread(paste0(base_dir, "/", "district_maxcatch.gz")), fill = TRUE)
  }

  # Write out the district max
  fwrite(dist_max, paste0(dir_name, "/distpreds_max.gz"))

  # Get district ttimes for communes --------------------------------------------------------
  dist_merge <- dist_max[, c("distcode", "ttimes_wtd", "scenario"),
    with = FALSE
  ][, setnames(.SD, "ttimes_wtd", "ttimes_wtd_dist")]

  # Match district ttimes to commune ids to get uniform expectations of bite inc across district
  comm_all$distcode <- substr(comm_all$commcode, 1, 7)
  comm_max$distcode <- substr(comm_max$commcode, 1, 7)
  comm_all <- comm_all[dist_merge, on = c("scenario", "distcode")]
  comm_max <- comm_max[dist_merge, on = c("scenario", "distcode")]

  # Write out the commune files with a look up var for predictions
  comm_all$lookup <- paste("scenario", comm_all$scenario, sep = "_")
  comm_max$lookup <- paste("scenario", comm_max$scenario, sep = "_")

  fwrite(comm_all, paste0(dir_name, "/commpreds_all.csv"))
  fwrite(comm_max, paste0(dir_name, "/commpreds_max.csv"))
}

# Separate function for aggregate to admin ----------------------------------------------------
aggregate_admin <- function(base_df, admin = "distcode", scenario) {
  setDTthreads(1)

  base <- copy(base_df)
  base[, c("pop_admin", "pop_wt") :=
    .(sum(pop, na.rm = TRUE), sum(pop[!is.na(ttimes)], na.rm = TRUE), scenario),
  by = get(admin)
  ]

  admin_df <-
    base[, .(
      ttimes_wtd = sum(ttimes * pop, na.rm = TRUE),
      ttimes_un = sum(ttimes, na.rm = TRUE),
      ncells = .N,
      prop_pop_catch = sum(pop, na.rm = TRUE) / pop_wt[1],
      pop_wt = pop_wt[1],
      pop_admin = pop_admin[1]
    ),
    by = .(get(admin), catchment)
    ] # first by catchment
  admin_df[, c("ttimes_wtd", "ttimes_un", "scenario") :=
    .(
      sum(ttimes_wtd, na.rm = TRUE) / pop_wt,
      sum(ttimes_un, na.rm = TRUE) / sum(ncells, na.rm = TRUE),
      scenario
    ), by = get] # then by district

  admin_df <- admin_df[!is.na(catchment)]

  setnames(admin_df, "get", admin)

  return(admin_df)
}

# Candidate functions here
prop_wtd <- function(base_df, cand_ttimes, thresh_ttimes) {
  inds <- which(cand_ttimes < base_df$ttimes & base_df$ttimes >= thresh_ttimes
                & !is.na(base_df$prop_pop))
  sum(base_df$prop_pop[inds] * # pop affected
        ((base_df$ttimes[inds] - cand_ttimes[inds]) / base_df$ttimes[inds]),
      na.rm = TRUE
  ) # weighted by reduction
}

prop <- function(base_df, cand_ttimes, thresh_ttimes) {
  inds <- which(cand_ttimes < base_df$ttimes & base_df$ttimes >= thresh_ttimes
                & !is.na(base_df$prop_pop))
  sum(base_df$prop_pop[inds], na.rm = TRUE) # prop pop only
}

mean_tt <- function(base_df, cand_ttimes, thresh_ttimes) {
  inds <- which(cand_ttimes < base_df$ttimes & base_df$ttimes >= thresh_ttimes
                & !is.na(base_df$prop_pop))
  mean(base_df$ttimes[inds] - cand_ttimes[inds], na.rm = TRUE) # prop pop only
}
