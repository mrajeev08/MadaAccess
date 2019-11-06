###################################################################################################
##' Making maps for figure 1 of ctar and patient locations
##' Details: network style figure for vizualizing where patients are reporting to in each 
##' district
##' Author: Malavika Rajeev 
####################################################################################################
rm(list = ls())
source("R/02_bitedata/03_estimate_biteinc.R") # either source this or output bite data

## Libraries (not called in previous script)
library(patchwork)

## Read in raw data (not processed)
national <- read.csv("data/processed/bitedata/national.csv")
moramanga <- read.csv("data/processed/bitedata/moramanga.csv")

##' N. Matching up metadata
##' ------------------------------------------------------------------------------------------------
ctar_coords <- SpatialPoints(cbind(ctar_metadata$LONGITUDE, ctar_metadata$LATITUDE), 
                             proj4string = 
                               CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ctar_metadata$commcode <- over(ctar_coords, mada_communes)$ADM3_PCODE
ctar_metadata$distcode <- over(ctar_coords, mada_districts)$distcode

## Centers with no data ## Change this to be so that only ones with zero forms!
no_data <- c("Fianarantsoa", "Ambatomainty", "Ambovombe Androy", "Tsiroanomandidy", 
             "Taolagnaro", "Mandritsara", 
             "Antsiranana", "Marolambo", "Nosy be", "Sainte Marie", "Vangaindrano")
ctar_metadata$exclude <- 0
ctar_metadata$exclude[ctar_metadata$CTAR %in% no_data] <- 1
ctar_metadata$color <- c("#FCC56F","#FFDBE5", "#7A4900", "#CBCDD2", "#0000A6", "#EFF2F1",
                         "#99d8c9", "#B79762", "#004D43", "#8FB0FF", "white", "#FD9C54", "#8362B5",
                         "#578FB0","#5A0007", "#809693", "#D16100", "#1B4400", "#4FC601", "#3B5DFF", 
                         "#4A3B53", "#FF2F80","#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92",
                         "#FF90C9", "#B903AA", "#FEFFE6", "#E9E9D2")
ctar_metadata$fill <- ctar_metadata$color
ctar_metadata$color[ctar_metadata$CTAR %in% no_data] <- "black"

catch_cols <- ctar_metadata$color
names(catch_cols) <- ctar_metadata$CTAR
catch_fills <- ctar_metadata$fill
names(catch_fills) <- ctar_metadata$CTAR

##' 2. Mapping raw data: National at district level
##' ------------------------------------------------------------------------------------------------
national %>% 
  filter(year(date_reported) > 2013, 
         year(date_reported) < 2018, type == "new", !is.na(distcode), !is.na(id_ctar)) %>%
  group_by(distcode) %>%
  summarize(count = n()) %>%
  left_join(select(mada_districts@data, long, lat, distcode = distcode, ctar = ctch_ttwtd)) %>%
  left_join(select(ctar_metadata, ctar = CTAR, color, fill)) -> dist_pts
                   
national %>%
  group_by(id_ctar) %>%
  summarize(count = n()) %>%
  left_join(select(ctar_metadata, ctar = CTAR, distcode, id_ctar, color, fill)) %>%
  left_join(select(mada_districts@data, distcode, long, lat)) -> ctar_pts

national %>%
  group_by(distcode, id_ctar) %>%
  summarize(count = n()) %>%
  left_join(select(mada_districts@data, to_long = long, to_lat = lat, distcode)) %>%
  left_join(select(ctar_metadata, id_ctar, ctar = CTAR, color, fill, ctar_distcode = distcode)) %>%
  left_join(select(mada_districts@data, from_long = long, from_lat = lat, 
                    ctar_distcode = distcode))-> ctar_todist_lines

ctar_todist_lines %>%
  ungroup() %>%
  mutate(end_lat_plus = to_lat + log(count + 1)*0.05, 
         end_long_plus = to_long + log(count + 1)*0.05,
         end_lat_minus = to_lat - log(count + 1)*0.05, 
         end_long_minus = to_long - log(count + 1)*0.05,
         group_id = paste0(distcode, id_ctar)) %>%
  select(group_id, contains("lat"), contains("long"), 
         -to_lat, -to_long, color, ctar) -> ctar_todist_pols

ctar_todist_pols <- bind_rows(select(ctar_todist_pols, lat = end_lat_plus, long = end_long_plus,
                                     group = group_id, color, ctar),
                              select(ctar_todist_pols, lat = end_lat_minus, long = end_long_minus,
                                     group = group_id, color, ctar),
                              select(ctar_todist_pols, lat = from_lat, long = from_long, 
                                     group = group_id, color, ctar))

mada_districts$color <- ctar_metadata$color[match(mada_districts$ctch_ttwtd, ctar_metadata$CTAR)]
gg_district <- fortify(mada_districts, region = "distcode")
gg_district %>% 
  left_join(select(mada_districts@data, distcode, color, ctar = ctch_ttwtd), 
            by = c("id" = "distcode")) -> gg_district

sizes <- log(c(100, 200, 400, 800, 1600) + 0.1)
fig1A <- ggplot() +
  geom_polygon(data = gg_district, 
               aes(x = long, y = lat, group = group, fill = ctar), color = "white", alpha = 0.5) +
  geom_polygon(data = filter(ctar_todist_pols, color != "black"), aes(x = long, y = lat, group = group,
                                     fill = ctar), color = NA, alpha = 0.5, 
               show.legend = FALSE) +
  geom_point(data = dist_pts, aes(x = long, y = lat, fill = ctar,
                                       size = log(count + 0.1)), color = alpha("black", 0.75),
             shape = 21, alpha = 0.75) +
  geom_point(data = ctar_pts, aes(x = long, y = lat, 
                                     size = log(count + 0.1), color = ctar), 
             shape = 1, stroke = 1.2) +
  scale_fill_manual(values = catch_cols, guide = "none") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_size_identity("Reported bites", labels = as.character(c(100, 200, 400, 800, 1600)),
                      breaks = sizes, guide = "legend") +
  guides(size = guide_legend(override.aes = list(color = "grey50"))) +
  labs(tag = "A") +
  theme_void()

##' 3. Mapping raw data: Moramanga at commune level
##' ------------------------------------------------------------------------------------------------
moramanga %>%
  mutate(date_reported = ymd(date_reported)) %>%
  filter(date_reported >= "2016-10-01", date_reported < "2019-05-01", 
         type == "new") %>%
  group_by(commcode) %>%
  summarize(count = n()) %>%
  left_join(select(mada_communes@data, long, lat, commcode = ADM3_PCODE, ctar = ctch_ttwtd)) %>%
  left_join(select(ctar_metadata, ctar = CTAR, color, fill)) -> comm_pts
comm_pts$from_long <- ctar_metadata$LONGITUDE[ctar_metadata$CTAR == "Moramanga"]
comm_pts$from_lat <- ctar_metadata$LATITUDE[ctar_metadata$CTAR == "Moramanga"]

comm_pts %>%
  ungroup() %>%
  mutate(end_lat_plus = lat + log(count + 1)*0.05, 
         end_long_plus = long + log(count + 1)*0.05,
         end_lat_minus = lat - log(count + 1)*0.05, 
         end_long_minus = long - log(count + 1)*0.05,
         group_id = commcode) %>%
  select(group_id, contains("lat"), contains("long"), -lat, -long, color, ctar) -> ctar_tocomm_pols

ctar_tocomm_pols <- bind_rows(select(ctar_tocomm_pols, lat = end_lat_plus, long = end_long_plus,
                                     group = group_id, color, ctar),
                              select(ctar_tocomm_pols, lat = end_lat_minus, long = end_long_minus, 
                                     group = group_id, color, ctar),
                              select(ctar_tocomm_pols, lat = from_lat, long = from_long, 
                                     group = group_id, color, ctar))

mada_communes$color <- ctar_metadata$color[match(mada_communes$ctch_ttwtd, ctar_metadata$CTAR)]
gg_commune <- fortify(mada_communes, region = "ADM3_PCODE")
gg_commune %>% 
  left_join(select(mada_communes@data, ADM2_EN, ADM3_PCODE, color, ctar = ctch_ttwtd), 
            by = c("id" = "ADM3_PCODE")) %>%
  left_join(select(comm_pts, commcode, count), by = c("id" = "commcode")) %>%
  filter(ctar == "Moramanga") -> gg_commune_plot

## TO DO: add inset with districts included in ment outlined (or other way of showing what this refers to)
fig1B <- ggplot() +
  geom_polygon(data = gg_commune_plot, 
               aes(x = long, y = lat, group = group), fill = "#4A3B53", 
               color = "white", alpha = 0.5) +
  geom_polygon(data = ctar_tocomm_pols, aes(x = long, y = lat, group = group,
                                     fill = ctar), color = NA, alpha = 0.5, 
               show.legend = FALSE) +
  geom_point(data = filter(comm_pts, commcode != "MG33314010"), aes(x = long, y = lat, fill = ctar,
                                  size = log(count + 0.1)), color = alpha("black", 0.75),
             shape = 21, alpha = 0.75) +
  geom_point(data = filter(comm_pts, commcode == "MG33314010"), aes(x = long, y = lat, 
                                  size = log(count + 0.1), color = ctar), 
             shape = 1, stroke = 1.2) +
  scale_fill_manual(values = catch_fills, guide = "none") +
  scale_color_manual(values = catch_fills, guide = "none") +
  scale_size_identity("Reported bites", labels = as.character(c(100, 200, 400, 800, 1600)),
                      breaks = sizes, guide = "legend") +
  guides(size = guide_legend(override.aes = list(color = "grey50"))) +
  theme_void()

insetB <-  ggplot() + 
  geom_polygon(data = gg_district, 
                        aes(x = long, y = lat, group = group), color = "grey50", 
                        alpha = 0.5) + 
  geom_polygon(data = gg_commune_plot, aes(x = long, y = lat, group = group), 
                         fill= "#4A3B53") +
  labs(tag = "C") +
  theme_void()

## Bite incidence estimates
## Mada
bites %>%
  ## filter known contacts and estimated ones based on throughput
  filter(estimated_cat1 == 0, include == 1) %>% 
  group_by(year, distcode) %>%
  summarize(bites = n()) -> bites_district
bites_district$CTAR <- mada_districts$ctch_ttwtd[match(bites_district$distcode, 
                                                       mada_districts$distcode)]
bites_district$id_ctar<- ctar_metadata$id_ctar[match(bites_district$CTAR, ctar_metadata$CTAR)]
bites_district %>%
  left_join(reporting) %>%
  filter(reporting > 0.25) %>% ## dont include any for which less than 25% reporting
  ## correct for reporting by year and ctar reported to 
  mutate(bites = bites/reporting) -> yearly_ests
  
mada_districts@data %>%
  select(group_name = distcode, district = ADM2_EN, pop, long, lat, 
         catchment = ctch_ttwtd, covar = ttms_wtd, exclude_by_ttimes,
         ctar_in_district) %>%
  filter(exclude_by_ttimes == 0) %>%
  mutate(covar_name = "ttimes", catch = as.numeric(droplevels(catchment)), 
         group = as.numeric(droplevels(group_name)), 
         names = group_name) %>%
  left_join(yearly_ests, by = c("group_name" = "distcode")) %>%
  arrange(group) -> bites_plot

district_cols <- ctar_metadata$fill
names(district_cols) <- ctar_metadata$CTAR

fig1C <- ggplot(bites_plot, 
                aes(x = reorder(district, -covar), 
                    y = bites/pop*1e5, color = catchment)) +
  geom_boxplot() +
  scale_color_manual(values = district_cols) + 
  ylab("Annual bites per 100k") +
  xlab("Districts ordered by \n travel times (high to low)") +
  theme(legend.position = "none", axis.text.y = element_blank()) +
  coord_flip() +
  labs(tag = "B")

## Moramanga
moramanga %>%
  mutate(month_date = floor_date(ymd(moramanga$date_reported), unit = "month")) %>%
  filter(known_cat1 == 0, type == "new", month_date >= "2016-10-01", 
         month_date <= "2019-06-01", !is.na(commcode)) %>%
  group_by(commcode, month_date) %>%
  summarize(bites = n()) %>%
  ungroup() %>%
  complete(month_date = seq(min(month_date), max(month_date), by = "month"), 
           commcode, fill = list(bites = 0)) %>%
  group_by(commcode) %>%
  summarize(avg_bites = mean(bites)*12) %>% # average monthly bites x 12 to get annual avg_bites
  complete(commcode = mada_communes$ADM3_PCODE, fill = list(avg_bites = 0)) -> mora_bites 

fig1D <- ggplot(morabites_by_ttimes, 
                aes(x = reorder(names, -covar), 
                    y = avg_bites/pop*1e5, color = catchment)) +
  geom_boxplot() +
  scale_color_manual(values = district_cols) + 
  ylab("Annual bites per 100k") +
  xlab("Communes ordered by \n travel times (high to low)") +
  coord_flip() +
  theme(legend.position = "none", axis.text.y = element_blank()) +
  labs(tag = "D")

## Save these as objects and read back in to make final figs using patchwork
figM1 <- fig1A + fig1C + {
    {
      {insetB +
        plot_spacer() +
        plot_layout(ncol = 1, heights = c(1, 3))
      } |  fig1B} + plot_layout(widths = c(1, 3))} +
     fig1D + plot_layout(ncol = 2, widths = c(2, 1), heights = c(1.5, 1))
  
ggsave("figs/M1.jpg", figM1, device = "jpg", height = 12, width = 8)

