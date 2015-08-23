setwd("/Users/GA/dropbox/Datascience/Universite_Johns-Hopkins/Exploration_analytique_de_donnees/RExplordata/Project2")

#----------------------------------------------------------
# How have emissions from motor vehicle sources changed from 1999???2008 in Baltimore City?
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
NEI_motor_vehicules_baltimore <- NEI[(NEI$SCC %in% SCC_motor_vehicules$SCC) & (NEI$fips == "24510"),]


#----------------------------------------------------------
# Building R object with emission by year
#----------------------------------------------------------
library(dplyr)

Year <- group_by(NEI_motor_vehicules_baltimore, year)
EmByYear <- summarise(Year, Emissions = sum(Emissions))
EmByYear

#----------------------------------------------------------
# Plot in "plot5.png file
#----------------------------------------------------------

library(ggplot2)

png(file = "plot5.png")  
qplot(year,Emissions, data = EmByYear, geom = c("point","smooth"))
dev.off()

