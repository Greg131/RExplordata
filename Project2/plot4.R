setwd("/Users/GA/dropbox/Datascience/Universite_Johns-Hopkins/Exploration_analytique_de_donnees/RExplordata/Project2")


#----------------------------------------------------------
# Across the United States, 
# how have emissions from coal combustion-related sources changed from 1999???2008?
#----------------------------------------------------------

#----------------------------------------------------------
# Loading data in memory
#----------------------------------------------------------
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")



#--------------------------------------------------------------
# Subsetting with data from coal combustion-related sources 
#--------------------------------------------------------------
levels(SCC$EI.Sector)
index_coal <- grep("Coal",levels(SCC$EI.Sector),fixed=TRUE, ignore.case = FALSE)
levels_coal <- levels(SCC$EI.Sector)[index_coal]
SCC_coal <- SCC[SCC$EI.Sector %in% levels_coal,]

NEI_Coal <- NEI[NEI$SCC %in% SCC_coal$SCC,]

#----------------------------------------------------------
# Building R object with emission by year
#----------------------------------------------------------
library(dplyr)

Year <- group_by(NEI_Coal, year)
EmByYear <- summarise(Year, Emissions = sum(Emissions))
EmByYear

#----------------------------------------------------------
# Plot in "plot4.png file
#----------------------------------------------------------

library(ggplot2)

png(file = "plot4.png")  
qplot(year,Emissions, data = EmByYear, geom = c("point","smooth"))
dev.off()

