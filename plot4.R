## Read source data
NEI <- readRDS("exdata-data-NEI_data/summarySCC_PM25.rds")
SCC <- readRDS("exdata-data-NEI_data/Source_Classification_Code.rds")

## Load the scales package
require(scales)

## Subset the SCC values.
## The subset is done by looking at SCC.Level.One & SCC.Level.Three variables.
## If the SCC.Level.One column has the word "combustion" and the SCC.Level.Three column has the word "coal" or "lignite", 
## the particular observation is choosen.
SCC.coal.comb <- SCC[grepl("combustion",SCC$SCC.Level.One, ignore.case=TRUE) & (grepl("coal",SCC$SCC.Level.Three,ignore.case=TRUE) | grepl("lignite", SCC$SCC.Level.Three, ignore.case=TRUE)), ]

## subset NEI database based on SCC values from SCC.coal.comb data frame.
NEI.coal.comb <- subset(NEI,NEI$SCC %in% SCC.coal.comb$SCC)

## Sum emissions based on year.
NEI.coal.comb.total <- tapply(NEI.coal.comb$Emissions,NEI.coal.comb$year, sum)

## convert to megatons
NEI.coal.comb.total <- NEI.coal.comb.total/1e+06

## Generate year information as a date
year <- as.Date(paste(names(NEI.coal.comb.total), "1", "1", sep="-"))

## Create breaks vector for the plot
breaks.vec <- c(seq(from=min(year),to=(max(year) + 365),by="3 years"))
            
## Draw a plot with a linear model.
qplot(y=NEI.coal.comb.total,x=year, geom = c("point", "smooth"), method="lm", color=I("red")) + 
    scale_x_date(breaks=breaks.vec, labels=date_format("%Y")) + 
    ylab("Emissions in megatons") + 
    labs(title="Emissions per year")