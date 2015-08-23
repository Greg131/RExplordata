setwd("/Users/GA/dropbox/Datascience/Universite_Johns-Hopkins/Exploration_analytique_de_donnees/RExplordata/Project2")


#----------------------------------------------------------
# Have total emissions from PM2.5 decreased in the Baltimore City, 
# Maryland (fips == "24510") from 1999 to 2008? 
# Use the base plotting system to make a plot answering this question.
#----------------------------------------------------------

#----------------------------------------------------------
# Loading data in memory
#----------------------------------------------------------
NEI <- readRDS("summarySCC_PM25.rds")

#--------------------------------------------------------------
# Subsetting with data of  Baltimore City, 
# Maryland (fips == "24510")
#--------------------------------------------------------------
NEI$fips
NEI_Baltimore <- NEI[NEI$fips == "24510",]

#----------------------------------------------------------
# Building R object with emission for each year
#----------------------------------------------------------
spNEIyear = split(NEI_Baltimore$Emissions,NEI_Baltimore$year)
PM2.5ByYear <- sapply(spNEIyear,sum)

#----------------------------------------------------------
# Plot in "plot2.png2 file
#----------------------------------------------------------
png(file = "plot2.png")  
par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1))
barplot(PM2.5ByYear, main = " PM2.5 emissions in Baltimore over 10 years",
        ylab = "PM2.5 Emissions in tons", density = 20,col = "steelblue")
dev.off()

