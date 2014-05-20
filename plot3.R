## Read source data
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

## Load the reshape2 package
require(reshape2)
require(scales)
require(grid)

## Melt the data with keeping Emissions as the only variable
NEI.melt <- melt(NEI, measure.vars=c("Emissions"))

## Sum the data back using dcast function. 
NEI.cast <- dcast(NEI.melt, type + year ~ variable, sum)

## conver year to column to date
NEI.cast$year <- as.Date(paste(NEI.cast$year, "1", "1", sep="-"))

## Create breaks vector for the plot
breaks.vec <- c(seq(from=min(NEI.cast$year),to=(max(NEI.cast$year) + 365),by="3 years"))

## Plot the data using ggplot
ggplot(data=NEI.cast, aes(x=year,y=Emissions/1e+06)) + 
    geom_point() + 
    facet_grid(.~type) + 
    geom_smooth(method="lm") + 
    ylab("Emissions in megatons") + 
    scale_x_date(breaks=breaks.vec, labels=date_format("%Y")) +
    theme(panel.margin=unit(0.7,"line")) +
    labs(title="Emissions per year")

## Second plot to generate colors for each type. DONT Submit!
ggplot(data=NEI.cast, aes(x=year,y=Emissions/1e+06,color=type)) + 
     geom_point() + 
     geom_smooth(method="lm") + 
     ylab("Emissions in megatons") + 
     scale_x_date(breaks=breaks.vec, labels=date_format("%Y")) +
     theme(panel.margin=unit(0.7,"line")) +
     labs(title="Emissions per year")
