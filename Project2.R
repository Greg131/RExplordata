setwd("/Users/GA/dropbox/Datascience/Universite_Johns-Hopkins/Exploration_analytique_de_donnees/RExplordata")


NEI <- readRDS("../data/NEI_data/summarySCC_PM25.rds")
?object.size()
object.size(SummarySCC_PM25)
head(SummarySCC_PM25)
tail(SummarySCC_PM25)
str(head(SummarySCC_PM25))

SCC <- readRDS("../data/NEI_data/Source_Classification_Code.rds")

#----------------------------------------------------------
# 1 - Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? 
# Using the base plotting system, make a plot showing 
# the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.
#----------------------------------------------------------