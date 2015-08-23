setwd("/Users/GA/dropbox/Datascience/Universite_Johns-Hopkins/Exploration_analytique_de_donnees/RExplordata")


NEI <- readRDS("../data/NEI_data/summarySCC_PM25.rds")
?object.size()
object.size(NEI)
object.size(NEI,units="Mb")
head(NEI)
tail(NEI)
str(head(NEI))

SCC <- readRDS("../data/NEI_data/Source_Classification_Code.rds")

#----------------------------------------------------------
# 1 - Have total emissions from PM2.5 decreased in the United States 
# from 1999 to 2008? 
# Using the base plotting system, make a plot showing 
# the total PM2.5 emission from all sources 
# for each of the years 1999, 2002, 2005, and 2008.
#----------------------------------------------------------

summary(NEI)
# searching for missing values
colSums(is.na(NEI))
all(colSums(is.na(NEI))==0)

NEI$Pollutant <- as.factor(NEI$Pollutant)
table(NEI$Pollutant)

NEI$SCC <- as.factor(NEI$SCC)
table(NEI$SCC)

NEI$type <- as.factor(NEI$type)
table(NEI$type)

NEI[NEI$SCC == "40201501",]
NEI[NEI$fips == "17201",]
sum(NEI$Emissions)

spNEIyear = split(NEI$Emissions,NEI$year)

PM2.5ByYear <- sapply(spyear,sum)
Embyyear
class(Embyyear)


barplot(Embyyear)

unclass(Embyyear)
