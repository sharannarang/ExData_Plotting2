## Read source data
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

## Extract baltimore data
baltimore_data <- NEI[NEI$fips == "24510", ]

## Split the data according to the year
year_data <- split(baltimore_data, baltimore_data$year)

## Sum each year data
pm25.year <- sapply(year_data, function(x) sum(x[,"Emissions"]))

## Alternate why to sum each year data. 
pm25.year <- tapply(baltimore_data$Emissions, baltimore_data$year, sum)

## convert tons to megatons for more manageable values.
pm25.year <- pm25.year/1e+06

png("plot2.png")

## Change background color
par(bg="grey")

## Create the plot. 
plot(as.numeric(names(pm25.year)), pm25.year, xlim=c(1999,2008), xaxp=c(1999, 2008, 3), ylab=expression("PM"[25]~" emissions (in megatons)"), xlab="Year", pch=20, col="blue")

## Create a linear model
l <- lm(pm25.year ~ as.numeric(names(pm25.year)))

## plot the linear model
abline(l)
title(main = expression("Baltimore Total PM"[25]~"per year"))

dev.off()