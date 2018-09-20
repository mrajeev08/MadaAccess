# rm everything 
rm(list=ls())
# library(maptools)
# library(sp)
# shapefile <- readShapePoly("your_shapefile", proj4string=CRS('+proj=longlat'))
# mergeFile <- sp::merge()
# SET-UP ------------------------------------------------------------------
##set wd
setwd('~/Documents/Code/MadaConsult')

# libraries
library(reshape2)
library(ISOweek)
library(lubridate)
library(maptools)
library(maps)
library(GISTools)
library(rgdal)
library(sp)
library(lattice)
library(spdep)
library(plyr) 
library(RColorBrewer)
library(tau)
library(gridExtra)
library(ggplot2)

# source code (function for getting consecutive dates)
source ('R/utils.R')

# data files
consult <- read.csv('data/MadaRabiesConsultation_09052016.csv', encoding = 'Latin1', header = TRUE, comment.char = '#')
# exclude ones for which center and district information is unknown
consult <- subset (consult, CTAR != "27")
consult <- subset (consult, CTAR != "28")
consult <- subset (consult, CTAR != "")

# # OPTIONAL TO EXCLUDE “ALIVE” DOGGIES -----------------------------------
# unhash to exclude dogs that were noted to be alive at time of reporting##
#consult <- subset (consult, Outcome != "vivant")

# Cornell 2002 census data
geocodes <- read.csv('data/census/geocodes.csv', header = TRUE, comment.char = '#')
pop <- read.csv('data/census/population.csv', header = TRUE, comment.char = '#')
agri <- read.csv('data/census/agriculture.csv', header = TRUE, comment.char = '#')
health <- read.csv('data/census/health.csv', header = TRUE, comment.char = '#')
infra <- read.csv('data/census/infrastructure.csv', header = TRUE, comment.char = '#')
transport <- read.csv('data/census/transport.csv', header = TRUE, comment.char = '#')

# bring in the shape file
madafile<-readShapePoly("data/gis/district_init.shp", proj4string=CRS('+proj=longlat'))

# getting district central points
coordinates <- read.csv('data/gis/coordinates.csv')

# merging by dist_codes for matching with census and other data
dist_key <- read.csv ('data/dist_key.csv')
dist_key <- merge (dist_key, coordinates[,c(1,2,8)], by.x="district_code", by.y="dis_code")
cent_key  <- read.csv ('data/center_key.csv')
cent_key <- merge (cent_key, coordinates[,c(1,2,8)], by.x="center_code", by.y="dis_code")
consult <- merge (consult, dist_key, by.x = 'District', by.y = 'district')
consult <- merge (consult, cent_key, by.x = 'CTAR', by.y = 'center')
consult <- rename (consult, c('X.x'="dist_X", 'Y.x'='dist_Y', 'X.y'="cent_X", 'Y.y'='cent_Y'))

# CONSULT DATA ------------------------------------------------------------
# getting consecutive months
consult$month <- get.consec(consult$DateConsult, format.date = '%m/%d/%y', start = "01-01-2010", 
                            year1 = 2010, tstep = 'month')

# pull out the year and month
library(lubridate)
consult$year <- year(as.Date (consult$DateConsult, format = "%m/%d/%y"))
consult$month_name <- month((as.Date (consult$DateConsult, format = "%m/%d/%y")), label = TRUE)

# getting no. of doses and days between doses
consult$day1 <- get.consec(consult$DateDose1, format.date = '%m/%d/%y', start = "01-01-2010", 
                            year1 = 2010, tstep = 'day')
consult$day2 <- get.consec(consult$DateDose2, format.date = '%m/%d/%y', start = "01-01-2010", 
                           year1 = 2010, tstep = 'day')
consult$day3 <- get.consec(consult$DateDose3, format.date = '%m/%d/%y', start = "01-01-2010", 
                           year1 = 2010, tstep = 'day')
consult$day4 <- get.consec(consult$DateDose4, format.date = '%m/%d/%y', start = "01-01-2010", 
                           year1 = 2010, tstep = 'day')
consult$day1[consult$day1 < 0] <- NA
consult$day2[consult$day2 < 0] <- NA
consult$day3[consult$day3 < 0] <- NA
consult$day4[consult$day4 < 0] <- NA

consult$dose1 <- NA
consult$dose1[!is.na(consult$day1)] <- 1
consult$dose1[is.na(consult$day1)] <- 0

consult$dose2 <- NA
consult$dose2[!is.na(consult$day2)] <- 1
consult$dose2[is.na(consult$day2)] <- 0

consult$dose3 <- NA
consult$dose3[!is.na(consult$day3)] <- 1
consult$dose3[is.na(consult$day3)] <- 0

consult$dose4 <- NA
consult$dose4[!is.na(consult$day4)] <- 1
consult$dose4[is.na(consult$day4)] <- 0

consult$doses <- consult$dose1 + consult$dose2 + consult$dose3 + consult$dose4

consult$dose1to2 <- consult$day2-consult$day1
consult$dose2to3 <- consult$day3-consult$day2
consult$dose3to4 <- consult$day4-consult$day3

consult$dose1to2[consult$dose1to2 < 0 | consult$dose1to2 > 120] <- NA
consult$dose2to3[consult$dose2to3 < 0 | consult$dose2to3 > 120] <- NA
consult$dose3to4[consult$dose3to4 < 0 | consult$dose3to4 > 120] <- NA

doses <- aggregate (consult[,c(53:73)], by = list(consult$district_code), FUN = mean, na.action = na.rm)

# getting oversus time series of consults
total.ts <- as.data.frame (table(consult$month))
plot (total.ts$Freq, type="l", xlab="Month", ylab="No. of Consults",
      bty= "l", main="No. Consults Nationally to CTAR, 2010-2013")

# AT THE CENTER LEVEL -----------------------------------------------------
# count of consulations by center per month 
center <- as.data.frame (table(consult$CTAR, consult$month))
names(center) <- c("center", "month", "Freq")
center$avg <- ave (center$Freq, center$center, FUN=mean)
center$ratio <- center$Freq/center$avg
center$ratio[center$ratio > 5] <- 5
center <- subset (center, center != "27")
center <- subset (center, center != "28")
center <- subset (center, center != "")
center <- merge (center, cent_key)
center$center <- with(center, reorder(center, X))

center_mat <- dcast (center, center ~ month, value.var="Freq")
center_mat$total <- rowSums(center_mat[,2:46])

# # plotting time series heat map
cent <- ggplot(center, aes(x=month, y=Freq, group=center)) + geom_line(aes(colour = center)) 
matcent<- ggplot(center, aes(month, center)) + geom_tile(aes(fill = Freq)) +  
  scale_fill_gradient(low="white", high="red") 
cent

# AT THE DISTRICT LEVEL ---------------------------------------------------
# count of consultations by district per month
district <- as.data.frame (table(consult$District, consult$month))
names(district) <- c("district", "month", "Freq")
district <- subset (district, district != "")
district <- merge (district, dist_key)
district$district <- with(district, reorder(district, X))

# time series heat mat
dist <- ggplot(district, aes(x=month, y=Freq, group=district)) + geom_line(aes(colour = district))
matdist<- ggplot(district, aes(month, district)) + geom_tile(aes(fill = Freq)) +  scale_fill_gradient(low="white", high="red") 
dist

# MERGING CENSUS AND CONSULT DATA @ DISTRICT LEVEL ------------------------
# getting population data
pop$code <- as.factor (substring (as.character(pop$Code.de.la.Commune),1,3))
health$code <- as.factor (substring (as.character(health$Code.de.la.Commune),1,3))
transport$code <- as.factor (substring (as.character(transport$Code.de.la.Commune),1,3))
infra$code <- as.factor (substring (as.character(infra$Code.de.la.Commune),1,3))
agri$code <- as.factor (substring (as.character(agri$Code.de.la.Commune),1,3))
transport$code <- as.factor (substring (as.character(transport$Code.de.la.Commune),1,3))

# district matrix
district_mat <- dcast (district, district ~ month, value.var="Freq")
district_mat$total <- rowSums(district_mat[,2:44])
district_mat$ave <- district_mat$total/43
district_mat <- merge(district_mat, dist_key)

# getting pop, livestock, poverty, roads, bites
pop <- dcast (pop, code ~., fun.aggregate = sum, value.var="Nombre.d.habitants")
agri <- aggregate (agri, by = list(agri$code), FUN = mean)
health <- aggregate (health, by = list(health$code), FUN = mean)
infra <- aggregate (infra, by = list(infra$code), FUN = mean)
transport <- aggregate (transport, by = list(transport$code), FUN = mean)
census_data <- as.data.frame(cbind (pop, agri, health, infra, transport))
census_data <- census_data[, c(1, 2, 7, 8, 9, 22, 23, 24, 35, 36, 37, 78, 80)]
names(census_data) <- c('code', 'pop', 'percent_ag', 'percent_fish', 'percent_livestock', 
                        'cattle', 'dairycows', 'pigs', 'percent_high_income', 'percent_middle_income', 
                        'percent_low_income', 'transport_cost', 'transport_duration')
district_mat <- merge (district_mat, census_data, by.x="district_code", by.y="code")
district_mat$consults_per1000 <- (district_mat$ave / district_mat$pop) * 100000

# PLOTTING SUMMARY DATA ---------------------------------------------------
# plotting by polygon value

# merging with shapefile data for plotting
check <- setdiff(madafile@data$dis_code, district_mat$district_code)
district_mat[101:114,] <- 0
district_mat$district_code[101:114] <- check
district_mat[101:114, 3:60] <- 0
district_mat[is.na(district_mat)] <- 0

district_mat <- district_mat[order(match(district_mat$district_code, madafile@data$dis_code)),]
rownames(district_mat) <- 0:113
setdiff(rownames(district_mat), rownames(madafile@data))
setdiff(rownames(district_mat), rownames(madafile@data))
madafile <- spCbind(madafile, district_mat)
madafile@data$bitesper1000 <- madafile@data$ave/madafile@data$Pop_adj15s*1e5
names(madafile)

# PANEL 1: total consults, average monthly consults, bite incidence, population 
par(mfrow=c(2,2))
hist(madafile@data$total, breaks=20)
hist(madafile@data$Pop_adj15s, breaks=20)
hist(madafile@data$ave, breaks=10)
hist(madafile@data$bitesper1000, breaks=30)

brks <- c(0, 0.000001, 1, 5, 10, 15, 20)
pal  <- c("#FFFFFF", "#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
bitesper1000 <- spplot(madafile, z="bitesper1000", at = brks, col.regions = pal, 
         main = "Monthly Bite Incidence\n(reported per 100,000 people)", 
         par.settings = list(axis.line = list(col =  'transparent')))

brks <- c( 0, 0.000001, 200, 400, 600, 800, 1000)
pal  <- c("#FFFFFF", "#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
total <- spplot(madafile, z="total", at = brks, col.regions = pal, 
         main = "Total number of consultations", 
         par.settings = list(axis.line = list(col =  'transparent')))

brks <- c( 0, 1e-30, 5, 10, 15, 20, 25)
pal  <- c("#FFFFFF", "#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
avemonthly <- spplot(madafile, z="ave", at = brks, col.regions = pal, 
              main = "Average Monthly Consults", 
              par.settings = list(axis.line = list(col =  'transparent')))
 
brks <- c( 0, 100000, 200000, 300000, 400000, 500000)
pal  <- c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
pop <- spplot(madafile, z="Pop_adj15s", at = brks, col.regions = pal, 
                     main = "District Population\n(Approximated for 2015)", 
                     par.settings = list(axis.line = list(col =  'transparent')))

# PANEL 2: transport cost, transport duration, %poor, cattle
par(mfrow=c(2,2))
hist(madafile@data$transport_cost, breaks=20)
hist(madafile@data$transport_duration, breaks=20)
hist(madafile@data$percent_low_income, breaks=20)
hist(madafile@data$percent_livestock, breaks=20)

brks <- c(0, 5e4, 1e5, 1.5e5, 2e5, 2.5e5)
pal  <- c( "#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
transport_cost <- spplot(madafile, z="transport_cost", at = brks, col.regions = pal, 
                       main = "Cost of Transport to Major City\n(Ariary)", 
                       par.settings = list(axis.line = list(col =  'transparent')))

brks <- c( 0, 20, 40, 60, 80)
pal  <- c( "#fee5d9","#fcae91","#fb6a4a","#de2d26","#a50f15")
transport_duration <- spplot(madafile, z="transport_duration", at = brks, col.regions = pal, 
                main = "Time to Major City\n(Hours)", 
                par.settings = list(axis.line = list(col =  'transparent')))

brks <- c( 0, 20, 40, 60, 80)
pal  <- c("#fee5d9","#fcae91","#fb6a4a","#de2d26","#a50f15")
percent_low <- spplot(madafile, z="percent_low_income", at = brks, col.regions = pal, 
                     main = "% Population Low-income", 
                     par.settings = list(axis.line = list(col =  'transparent')))

brks <- c( 0, 10, 20, 30, 40, 50)
pal  <- c("#fee5d9", "#fcbba1", "#fc9272", "#fb6a4a", "#de2d26", "#a50f15")
percent_livestock <- spplot(madafile, z="percent_livestock", at = brks, col.regions = pal, 
              main = "% Livestock Owners", 
              par.settings = list(axis.line = list(col =  'transparent')))

# MAPPING CONSULT NETWORK -------------------------------------------------
#Merging counts by center and district###
cent_dist <- as.data.frame (table(consult$center_code, consult$district_code))
cent_dist <- rename (cent_dist, c("Var1"="center_code", "Var2"="district_code"))

# getting district and center data for lines
cent_dist <- merge (cent_dist, dist_key)
cent_dist <- rename (cent_dist, c("X"="dist_X", "Y"= "dist_Y"))
cent_dist <- merge (cent_dist, cent_key)
cent_dist <- rename (cent_dist, c("X"="cent_X", "Y"= "cent_Y"))
cent_dist$size <- log(cent_dist$Freq + 0.1)*0.6

# getting colors for each center
center_cols <- c("#FFDBE5", "#7A4900", "#0000A6", "#63FFAC", "#B79762", "#004D43", "#8FB0FF", "#997D87",
                 "#5A0007", "#809693", "#FEFFE6", "#1B4400", "#4FC601", "#3B5DFF", "#4A3B53", "#FF2F80",
                 "#61615A", "#BA0900", "#6B7900", "#00C2A0", "#FFAA92", "#FF90C9", "#B903AA", "#D16100")

# center_points
center_points <- subset (cent_dist, as.numeric(as.character(cent_dist$center_code)) 
                         - as.numeric(as.character(cent_dist$district_code)) == 0)
cent_dist <- subset(cent_dist, Freq != 0)

# dist_points
dist_points <- subset (cent_dist, as.numeric(as.character(cent_dist$center_code)) 
                                        - as.numeric(as.character(cent_dist$district_code)) != 0)
dist_points <- dist_points[,c(2,3)]
dist_points <- aggregate (dist_points$Freq, by = list(dist_points$district_code), FUN = sum)
dist_points <- merge (dist_points, dist_key, by.x = "Group.1", by.y = "district_code" )
dist_points$size <- log(dist_points$x + 0.1)*0.6
circle_size <- c (log(100 + 0.1)*0.6, log(200 + 0.1)*0.6, log(400 + 0.1)*0.6, log(800 + 0.1)*0.6)



# PLOTS -------------------------------------------------------------------
matcent
ggsave("figs/jpeg/center_matrix_all.jpeg", width = 10, height = 10)

matdist
ggsave("figs/jpeg/district_matrix_all.jpeg", width = 10, height = 10)

jpeg("figs/jpeg/maps_1_all.jpeg", quality = 100, height = 1000, width = 1000)
grid.arrange (total, pop, avemonthly, bitesper1000)
dev.off()

jpeg("figs/jpeg/maps_2_all.jpeg", quality = 100, height = 1000, width = 1000)
grid.arrange (transport_cost, transport_duration, percent_low, percent_livestock)
dev.off()

jpeg("figs/jpeg/network_all.jpeg", quality = 100, height = 1000, width = 1000)
par(mfrow=c(1,1))
plot(madafile, col="black", border = "grey") 
points(center_points$cent_X, center_points$cent_Y, pch = 16, cex=center_points$size, col = "grey") 
points(dist_points$X, dist_points$Y, pch = 16, cex=dist_points$size, col = center_cols[center_points$center])
#points (center_points$cent_X, center_points$cent_Y, pch = ifelse(center_points$Freq == 0, 1, 16), 
 #cex = 1.5, col = center_cols[center_points$center])
#points (center_points$cent_X, center_points$cent_Y, pch = ifelse(center_points$Freq == 0, 1, 16), 
        #cex = 1.5, col = center_cols[center_points$center])
segments(cent_dist$cent_X, cent_dist$cent_Y, x1 = cent_dist$dist_X, y1 = cent_dist$dist_Y, 
         lwd = cent_dist$size*2, col = center_cols[cent_dist$center])
legend("topleft", c(">100", ">200", ">400", ">800"), title = "Number of total reported bites", 
       text.width = 3, pt.cex = circle_size, col = "grey",  pch = 16, inset = c(0.1, 0.1), lwd = 1.5, bty = "n", xpd = TRUE, y.intersp=2)
dev.off()


# Map of sites
site_cols <- c("green", "red", "blue")
sites <- rbind (dist_points[35, ], dist_points[16, ], dist_points[46, ])
plot(madafile, col="black", border = "grey")
points(sites$X, sites$Y, pch = 16, cex=3, col = site_cols)


#Data from Sep - Feb from Moramanga
reported <- c(17, 39, 71, 133, 84)
contacts <- c(0, 0, 12, 57, 3, NA)
traced <- c(6, 6, 11, 4, 26, 23)
normal <- c(2, 2, 4, 0, 13, 16)
suspect <- c (1, 1, 0, 3, 2, 2)
unknown <- c(3, 3, 5, 1, 11, 5)
months <- c("Sep", "Oct", "Nov", "Dec", "Jan", "Feb")

data <- rbind (normal, suspect, unknown)
colnames (data) <- months

barplot(data, col=c("darkblue","red", "gray"), ylab = "Cases", beside=FALSE)
legend("topright", rownames(data), inset = c(0.4, -0.35),
       fill = c("darkblue","red", "gray"), bty ="n", xpd = TRUE)

# Map of Moramanga with coords
moramanga <- subset(madafile, district == "Moramanga")
tracing_coords <- read.csv("~mrajeev/desktop/tracing_coords.csv", header=TRUE)
suspect <- subset (tracing_coords, Suspect == "Suspect")
confirmed <- subset (tracing_coords, Suspect == "Confirmed")
unknown <- subset (tracing_coords, Suspect == "Inconnu")
non <- subset (tracing_coords, Suspect == "Non")

plot(moramanga, col="black", border = "grey") 
points(non$long, non$lat, pch = 1, cex = 2, lwd = 2, col = "blue")
points(unknown$long, unknown$lat, pch = 1, cex = 2, lwd = 2, col = "gray")
points(suspect$long, suspect$lat, pch = 1, cex = 2, lwd = 2, col = "red")
points(confirmed$long, confirmed$lat, pch = 1, cex = 2, lwd = 2, col = "red")

