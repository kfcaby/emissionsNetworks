

emissions2005 <- emissions[ , as.Date(colnames(emissions)) >= "2005-01-01" & as.Date(colnames(emissions)) <= "2005-12-31"]

emissions2005 <- data.table(t(emissions2005), keep.rownames = TRUE)

names(emissions2005)[1] <- "date"

emissions2005[ , month := month(date)]


NA_by_month <- emissions2005[ , lapply(.SD, function(x) sum(is.na(x))), by = "month"]


pdf(file = "results/NAperMonth.pdf")
barplot(apply(NA_by_month,1, median), names.arg = 1:12, xlab = "Month", ylab = "Median number of NA 
        days per powerplant")
dev.off()

test <- emissions["PP1001",]

test_NA <- is.na(test)

rle(test_NA)

emissions[is.na(emissions)] <- 0
emissions[1:10,1:10]

#determine which monitors are every third day
load("data/daily_emissions_facility_temperature.RData")
head(PM)

PM2005 <- PM[ , as.Date(colnames(PM)) >= "2005-01-01" & as.Date(colnames(PM)) <= "2005-12-31"]

monitorNA <- rowSums(is.na(PM2005))
write.csv(monitorNA, file = "data/monitor2005_NAs.csv")
test <- fread(file = "data/monitor2005_NAs.csv")
head(test)


#mess around with temperature data

library(R.matlab)
library(arepa)
library(lubridate)
library(weathermetrics)
TEMP <- readMat(con = "data/REANALYSIS_air.sfc_DailyMean_zipcode_SUBSET_20050101_20051231.mat", fixNames = TRUE )$Result


zipWX <- fread(file = "data/zipcode_SUBSETSite.csv")
head(zipWX)

TEMP <- data.table(cbind(zipWX, t(TEMP)))
TEMP <- TEMP[complete.cases(TEMP),]
setkey(TEMP, zipcode)

monitor_locations <- fread(file = "data/monitor.locations.csv")
setkey(monitor_locations, ID)

linkage <- spatial_link_index(TEMP, "Latitude", "Longitude", "zipcode",
                              monitor_locations,"Latitude", "Longitude", "ID",
                              within = 100, closest = TRUE)
setkey(linkage,zipcode)

temperature_linked <- TEMP[linkage]
temperature_linked[ , zipcode := NULL]
temperature_linked[ , Latitude := NULL]
temperature_linked[ , Longitude := NULL]
temperature_linked[ , Distance := NULL]
monitors <- temperature_linked$ID
temperature_linked[ , ID := NULL]
temperature_linked <- as.matrix(temperature_linked)
rownames(temperature_linked) <- monitors
colnames(temperature_linked) <- seq.Date(as.Date("2015-01-01"),as.Date("2015-12-31"), by = "day")

temperature_linked <- kelvin.to.fahrenheit(temperature_linked)
#write.csv(temperature_linked, file = "data/temperature_complete.csv")


#edge percents
breaks = seq(0,5000, by = 250)
edges[ , distance_cat := cut(distance, breaks = breaks, dig.lab = 10)]

edgePerc_summary <- edges[ , list(edgePerc = round(sum(edge, na.rm = TRUE)/sum(!is.na(edge)),3),
              num_pairs = sum(!is.na(edge))),
      by = c("distance_cat", "receptor.region")]
setkey(edgePerc_summary, receptor.region,distance_cat)
edgePerc_summary

pdf(file = "results/edgePerc.pdf", width = 6.5, height = 6)
ggplot(data.frame(edgePerc_summary), aes(x = distance_cat, y = edgePerc, group = receptor.region, color = receptor.region)) + 
  geom_line()  + scale_color_discrete() + ylim(0,.35) +
  labs(x = "power plant to monitor distance (km)", y = "edge percent") + theme_bw() + 
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 90))
dev.off()

