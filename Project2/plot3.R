setwd("/Users/GA/dropbox/Datascience/Universite_Johns-Hopkins/Exploration_analytique_de_donnees/RExplordata/Project2")


#----------------------------------------------------------
# Of the four types of sources indicated by the type 
# (point, nonpoint, onroad, nonroad) variable, 
# which of these four sources have seen decreases 
# in emissions from 1999???2008 for Baltimore City? 
# Which have seen increases in emissions from 1999???2008? 
# Use the ggplot2 plotting system to make a plot answer this question.
#----------------------------------------------------------

#----------------------------------------------------------
# Loading data in memory
#----------------------------------------------------------
NEI <- readRDS("summarySCC_PM25.rds")

#--------------------------------------------------------------
# Subsetting with data of  Baltimore City, 
# Maryland (fips == "24510")
#--------------------------------------------------------------
NEI_Baltimore <- NEI[NEI$fips == "24510",]

#----------------------------------------------------------
# Building R object with emission by type & year
#----------------------------------------------------------
library(dplyr)

YearType <- group_by(NEI_Baltimore, year, type)
EmByTypeYear <- summarise(YearType, Emissions = sum(Emissions))
EmByTypeYear

#----------------------------------------------------------
# Plot in "plot3.png2 file
#----------------------------------------------------------

library(ggplot2)

png(file = "plot3.png")  
qplot(year,Emissions, data = EmByTypeYear, color = type, geom = c("point","smooth"))
dev.off()
