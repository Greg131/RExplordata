setwd("/Users/GA/dropbox/Datascience/Universite_Johns-Hopkins/Exploration_analytique_de_donnees/RExplordata/Project2")


#----------------------------------------------------------
# 1 - Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? 
# Using the base plotting system, make a plot showing 
# the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.
#----------------------------------------------------------

#----------------------------------------------------------
# Loading data in memory
#----------------------------------------------------------
NEI <- readRDS("summarySCC_PM25.rds")

#----------------------------------------------------------
# Building R object with emission for each year
#----------------------------------------------------------
spNEIyear = split(NEI$Emissions,NEI$year)
PM2.5ByYear <- sapply(spNEIyear,sum)

#----------------------------------------------------------
# Plot in "plot1.png2 file
#----------------------------------------------------------
png(file = "plot1.png")  
par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1))
barplot(PM2.5ByYear, main = " Total PM2.5 emissions in the US over 10 years",
        ylab = "PM2.5 Emissions in tons", density = 20,col = "steelblue")
dev.off()

