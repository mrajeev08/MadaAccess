#' CSB I per II in each commune
#' 
#' Locations of CSB (either level 1 or 2) in highest density area for each commune (n = 1371)
#' 
#' @format A data frame with 1371 rows and 9 variables:
#' 
#' \describe{ 
#'   \item{distcode}{District id corresponding to `distcode` column in `mada_districts` shapefile}
#'   \item{district}{District name corresponding to `district` column in `mada_districts` shapefile}
#'   \item{commcode}{Commune id corresponding to `commcode` column in `mada_communes` shapefile}
#'   \item{commune}{Communes name corresponding to `commcode` column in `mada_communes` shapefile}
#'   \item{long}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{type}{Type of clinic, CSB1 = 85, CSB2 = 1286}
#'   \item{pop_dens}{Density of people in grid cell where clinic is located (estimated from `pop1x1`)}
#'   \item{clinic_id}{clinic id used in analysis of expanding PEP}
#' }
#' 
#' @source Full data on all geolocated clinics available upon request from the IPM GIS unit; raw data are *not* shared here
#' 
"clin_per_comm"


#' CSB II per district
#' 
#' Locations of CSB II in location with highest population density for each district (n = 83)
#' 
#' @format A data frame with 83 rows and 9 variables:
#' 
#' \describe{ 
#'   \item{district}{District name corresponding to `district` column in `mada_districts` shapefile}
#'   \item{distcode}{District id corresponding to `distcode` column in `mada_districts` shapefile}
#'   \item{commune}{Communes name corresponding to `commcode` column in `mada_communes` shapefile}
#'   \item{commcode}{Commune id corresponding to `commcode` column in `mada_communes` shapefile}
#'   \item{long}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{type}{Type of clinic, all CSB II (CSB2)}
#'   \item{pop_dens}{Density of people in grid cell where clinic is located (estimated from `pop1x1}
#'   \item{clinic_id}{clinic id used in analysis of expanding PEP}
#' }
#' 
#' @source Full data on all geolocated clinics available upon request from the IPM GIS unit; raw data are *not* shared here
#' 
"clin_per_dist"


#' CSB II locations in Madagascar
#' 
#' Location data of Centre de Sante Niveau II (CSB II), a subset of public clinics and hospitals, in Madagascar. CSB II are larger and generally have more staff and services than CSB I (also public).
#' 
#' @format A data frame with 1648 rows and 9 variables:
#' 
#' \describe{ 
#'   \item{district}{Name of district where CSB II is located}
#'   \item{distcode}{District id corresponding to distcode column in `mada_districts` shapefile}
#'   \item{commune}{Commune id corresponding to commcode column in `mada_communes` shapefile}
#'   \item{commcode}{Name of commune where CSB II is located}
#'   \item{long}{Longitude}
#'   \item{lat}{Latitude}
#'   \item{type}{Type of clinic, all 'CSB II'}
#'   \item{pop_dens}{Density of people in grid cell where clinic is located (estimated from `pop1x1`)}
#'   \item{clinic_id}{clinic id used in analysis of expanding PEP}
#' }
#' 
#' @source Full data on all geolocated clinics available upon request from the IPM GIS unit; raw data are *not* shared here.
#' 
"csb2"


#' Metadata on ARMC in Madagascar
#' 
#' This data includes information on the location and doses provisioned to ARMC across Madagascar
#' 
#' @format A data frame with 31 rows and 21 variables:
#' 
#' \describe{ 
#'   \item{CTAR}{Name of ARMC}
#'   \item{District}{District that ARMC is located in}
#'   \item{id_ctar}{The numeric id for the ARMC used in the bite patient datasets}
#'   \item{year_opened}{The year that PEP provisioning began at the ARMC}
#'   \item{doses_2010}{Number of vials provisioned in 2010}
#'   \item{doses_2011}{Number of vials provisioned in 2011}
#'   \item{doses_2012}{Number of vials provisioned in 2012}
#'   \item{doses_2013}{Number of vials provisioned in 2013}
#'   \item{doses_2014}{Number of vials provisioned in 2014}
#'   \item{doses_2015}{Number of vials provisioned in 2015}
#'   \item{doses_2016}{Number of vials provisioned in 2016}
#'   \item{doses_2017}{Number of vials provisioned in 2017}
#'   \item{color}{The color used in figures for each ARMC}
#'   \item{notes}{Miscellaneous notes, two of the coastal ARMC locations were offset to}
#'   \item{long}{NA}
#'   \item{lat}{NA}
#'   \item{commcode}{Commune id corresponding to `commcode` column in `mada_communes` shapefile}
#'   \item{distcode}{District id corresponding to `distcode` column in `mada_districts` shapefile}
#'   \item{district}{District name corresponding to `district` column in `mada_districts` shapefile}
#'   \item{commune}{Communes name corresponding to `commcode` column in `mada_communes` shapefile}
#'   \item{clinic_id}{Clinic id used in analyis of expanding PEP access}
#' }
#' 
#' @source IPM Vaccine Clinic
#' 
"ctar_metadata"


#' Friction surface masked to Madagascar
#' 
#' Friction surface from Weiss et al. 2015 (linked in source), accessed through the malariaAtlas R package.
#' 
#' @format A 1639 x 877 raster at a resolution of 30 arcsec (approximately 1x1 km @@ equator)
#' 
#' @source [A global map of travel time to cities to assess inequalities in accessibility in 2015](https://www.nature.com/articles/nature25181/)
#' @source [Link to download friction surface from Malaria Atlas Project (MAP)](https://malariaatlas.org/geoserver/ows?service=CSW&version=2.0.1&request=DirectDownload&ResourceId=Explorer:2015_friction_surface_v1_Decompressed)
#' 
"friction_masked"
#' Administrative level 3 boundaries (commune) for Madagascar
#' 
#' sf object cleaned from original OCHA shapefile from 2018-10-31.
#' 
#' @format A simple feature object (from package `sf`) with data with 1579 rows and 7 variables:
#' 
#' \describe{ 
#'   \item{distcode}{District id}
#'   \item{district}{District name}
#'   \item{commcode}{commune id}
#'   \item{commune}{commune name}
#'   \item{long_cent}{Longitude of polygon centroid (approximate)}
#'   \item{lat_cent}{Latitude of polygon centroid (approximate)}
#'   \item{geometry}{sf attribute storing spatial coordinate data}
#' }
#' 
#' @source [United Nations Office for the Coordination of Humanitarian Affairs (OCHA) Regional Office for Southern Africa](https://data.world/ocha-rosa/26fa506b-0727-4d9d-a590-d2abee21ee22)
#' 
"mada_communes"


#' Administrative level 2 boundaries (District) for Madagascar
#' 
#' sf object cleaned from original OCHA shapefile, retreived from HDX on 2018-10-31. The district polygons for Antananarivo were dissolved to a single polygon to correspond to administrative boundaries at time period of data.
#' 
#' @format A simple feature object (from package `sf`) with data with 114 rows and 5 variables:
#' 
#' \describe{ 
#'   \item{distcode}{District id}
#'   \item{district}{District name}
#'   \item{long_cent}{Longitude of polygon centroid (approximate)}
#'   \item{lat_cent}{Latitude of polygon centroid (approximate)}
#'   \item{geometry}{sf attribute storing spatial coordinate data}
#' }
#' 
#' @source [United Nations Office for the Coordination of Humanitarian Affairs (OCHA) Regional Office for Southern Africa](https://data.world/ocha-rosa/26fa506b-0727-4d9d-a590-d2abee21ee22)
#' 
"mada_districts"


#' Moramanga ARMC patient data
#' 
#' Data on date of first report, home district (and commune when available) of patient, and ctar reported to from Moramanga ARMC 2016 - 2019, serving the Moramanga District and surrounding catchments
#' 
#' @format A data frame with 2284 rows and 8 variables:
#' 
#' \describe{ 
#'   \item{date_reported}{Date patient reported, 0\% NA}
#'   \item{type}{type of patient consultation: new, transfer; transfer is when a patient reported to another ARMC first; 0\% NA}
#'   \item{ctar}{Name of Anti-rabies medical center (ARMC) where patient reported to, 0\% NA}
#'   \item{id_ctar}{Numeric id of ARMC corresponding to column in `ctar_metadata`, 0\% NA}
#'   \item{distcode}{District id corresponding to `distcode` column in `mada_districts` shapefile, 0.96\% NA}
#'   \item{commcode}{Commune id corresponding to `commcode` column in `mada_communes` shapefile, 0.96\% NA}
#'   \item{known_cat1}{Whether patient reporting was seeking PEP for a Category I exposure by WHO classification, 1 indicates they yes0\% NA}
#'   \item{source}{The source of the data, the Moramanga Rabies Project}
#' }
#' 
#' @source [Data from previously published study, Rajeev et al. 2019 and continued data collection using the same methods](https://www.sciencedirect.com/science/article/pii/S0264410X18315202)
#' 
"moramanga"


#' Patient reported travel times from Moramanga ARMC
#' 
#' Data on self reported travel times in hours for a subset of patients reporting to the Moramanga ARMC between 2016 - 2019. and what mode of transport they used (mutliple options per patient possible).
#' 
#' @format A data frame with 2284 rows and 12 variables:
#' 
#' \describe{ 
#'   \item{car}{patient traveled by car? (1 indicates yes)}
#'   \item{Motorbike}{patient traveled by motorbike? (1 indicates yes)}
#'   \item{Ambulance}{patient traveled by ambulance? (1 indicates yes)}
#'   \item{foot}{patient traveled by foot? (1 indicates yes)}
#'   \item{Bus}{patient traveled by bus? (1 indicates yes)}
#'   \item{Bicycle}{patient traveled by bicycle? (1 indicates yes)}
#'   \item{Pus}{patient traveled by bicycle taxi (pousse-pousse)? (1 indicates yes)}
#'   \item{Other}{patient traveled by other means of transport? (1 indicates yes)}
#'   \item{hours}{Travel time from patient home to clinic in hours}
#'   \item{known_cat1}{Whether the patient was reporting for a Category I exposure by WHO classification}
#'   \item{commcode}{Commune id corresponding to `commcode` column in `mada_communes` shapefile}
#'   \item{distcode}{District id corresponding to `distcode` column in `mada_districts` shapefile}
#' }
#' 
#' @source [Data from previously published study, Rajeev et al. 2019 and continued data collection using the same methods](https://www.sciencedirect.com/science/article/pii/S0264410X18315202)
#' 
"mora_ttimes"


#' National ARMC patient data
#' 
#' Data on date of first report, home district (and commune when available) of patient, and ctar reported to from 24 ARMCbetween 2006 - 2019, serving multiple districts in Madagascar. Access to the anonymized databases are available through data sharing agreements with IPM. These raw databases are **not** available publicly.
#' 
#' @format A data frame with 96670 rows and 7 variables:
#' 
#' \describe{ 
#'   \item{date_reported}{Date patient reported, 0\% NA}
#'   \item{type}{type of patient consultation: new, transfer; transfer is when a patient reported to another ARMC first; 0.06\% NA}
#'   \item{ctar}{Name of Anti-rabies medical center (ARMC) where patient reported to, 0\% NA}
#'   \item{id_ctar}{Numeric id of ARMC corresponding to column in `ctar_metadata`, 0.18\% NA}
#'   \item{distcode}{District id corresponding to `distcode` column in `mada_districts` shapefile, 0.1\% NA}
#'   \item{commcode}{Commune id corresponding to `commcode` column in `mada_communes` shapefile, 54.3\% NA. Note that these were fuzzy matched and are largely incomplete and may be incorrect.}
#'   \item{source}{The source of the data, either the peripheral RedCap database, which holds data for all ARMC excluding the IPM ARMC, or the IPM database}
#' }
#' 
#' @source Data cleaned from RedCap databases maintained by IPM and the Ministry of Health which hold bite patient data from peripheral ARMC (outside of Antananarivo) and IPM (within Antananarivo) ARMC
#' 
"national"


#' World Pop aggregated to friction surface 
#' 
#' World Pop 2015 dataset for Madagascar aggregated/resampled to the MAP friction surface. See `data-raw/src/01_rasters.R` for code. We used the older version of the dataset using Linaird et al. 2012 as it generated more comparable estimates to recent national and microcensus data.
#' 
#' @format A 1639 x 877 raster at a resolution of 30 arcsec (approximately 1x1 km @@ equator), the same raster as the friction surface
#' 
#' @source [Population Distribution, Settlement Patterns and Accessibility across Africa in 2010](https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0031743)
#' 
"pop1x1"
