setwd("/Users/GA/dropbox/Datascience/Universite_Johns-Hopkins/Exploration_analytique_de_donnees/RExplordata/Project2")

#----------------------------------------------------------
# Compare emissions from motor vehicle sources in Baltimore City 
# with emissions from motor vehicle sources in 
# Los Angeles County, California (fips == "06037"). 
# Which city has seen greater changes over time in motor vehicle emissions?
#----------------------------------------------------------

#----------------------------------------------------------
# Loading data in memory
#----------------------------------------------------------
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")


#--------------------------------------------------------------
# Subsetting with motor vehicle sources + Baltimore City (fips == "24510")
#--------------------------------------------------------------
# Looking in EI.Sector
levels(SCC$EI.Sector)  
index <- grep("Vehicles",levels(SCC$EI.Sector), ignore.case = TRUE)
levels <- levels(SCC$EI.Sector)[index]
levels
# 4 EI.sectors corresponding to motor vehicle sources

# Subsetting SCC accordingly 
SCC_motor_vehicules <- SCC[SCC$EI.Sector %in% levels,]
# Subsetting NEI
NEI_motor_vehicules_baltimore_and_LA <- NEI[(NEI$SCC %in% SCC_motor_vehicules$SCC) & (NEI$fips %in%  c("24510","06037")),]


#----------------------------------------------------------
# Building R object with emission by year
#----------------------------------------------------------
library(dplyr)

# transforming fips in city names for ploting purpose
NEI_motor_vehicules_baltimore_and_LA[NEI_motor_vehicules_baltimore_and_LA$fips == "24510",]$fips <- "Baltimore City" 
NEI_motor_vehicules_baltimore_and_LA[NEI_motor_vehicules_baltimore_and_LA$fips == "06037",]$fips <- "Los Angeles County" 

Year <- group_by(NEI_motor_vehicules_baltimore_and_LA, year, fips)
EmByYearFips <- summarise(Year, Emissions = sum(Emissions))
EmByYearFips

#----------------------------------------------------------
# Plot in "plot6.png file
#----------------------------------------------------------

library(ggplot2)

png(file = "plot6.png")  
qplot(year,Emissions, data = EmByYearFips, facets =.~fips, geom = c("point","smooth"))
dev.off()

