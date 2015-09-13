# ----------------------------------------------------------
# R Exploratory Data Analysis
# ----------------------------------------------------------

setwd("~/Dropbox/Datascience/Universite_Johns-Hopkins/Exploration_analytique_de_donnees/RExplordata")
getwd() # Get working directory 

if (!file.exists("../data"))       {
  dir.create("../data")
}

pollution <- read.csv("../data/avgpm25.csv", colClasses = c("numeric","character","factor",
                                                            "numeric","numeric"))
?head
head(pollution)
tail(pollution)
str(pollution)
summary(pollution)
# ----------------------------------------------------------
# Simple Summaries of the data 
# 1 dim : summary, boxplot (boites a moustache), histogram, barplot, 
#       table (pour les variables categorielles)
# 2 dim : multiple/overlayed 1-D plots, Scaterplots
# >2 dim : multiple/overlayed 2-D plots, add Color, size , shape
#       spinning plots, 3-D plots
# ---------------------------------------------------------

# ---------------------------------------------------------
# 1 Dimension
# ---------------------------------------------------------

# Selon que les variables sont categorielles (ordonnees ou non)
# ou continues (ordonees ou cycliques.. type angle)

# 5 Number summary (min, max, median, 1st qu, 3rd qu). R add mean
summary(pollution$pm25)
summary(pollution)
table(pollution$region) # Pour les factors / variables categorielles


# Boxplot (1 variables numeriques...)
boxplot(pollution$pm25, col = "blue")
par("col") #valeur par defaut
colors() # donne la leste de couleurs....
boxplot(pollution$pm25, col = "steelblue")

# Histogram (1 variable numerique)
hist(pollution$pm25, col = "green") # freq est le nb de points...
rug(pollution$pm25) # plot all the points

hist(pollution$pm25, col = "green", breaks = 100)  
# Pas trop petit pour voir la courbe de distrib
# Pas trop grand sinon bruit sur la courbe
rug(pollution$pm25) # plot all the points

?hist()

hist(pollution$pm25, freq = F, col = "green", breaks = 20)  # freq = F -> densit?? de proba donc surface fixe de 1...
hist(pollution$pm25, density = 10, col = "green", breaks = 20)  # densit?? pour le coloriage...
hist(pollution$pm25, density = 20,col = "steelblue", breaks = 20, border = "red")  # border couleur...
hist(pollution$pm25, density = 20,col = "steelblue", breaks = 20, border = "red",xlim = c(7,12))  # border couleur...
hist(pollution$pm25, density = 20,col = "steelblue", breaks = 20, border = "red",xlim = c(0,80))  # border couleur...

# Boxplot
?boxplot()
boxplot(pollution$pm25, col = "steelblue")
boxplot(pollution$pm25, range =  0 , col = "steelblue")
boxplot(pollution$pm25, range =  1 , col = "steelblue")
boxplot(pollution$pm25, range =  2 , col = "steelblue")
boxplot(pollution$pm25, range =  0.5 , col = "steelblue") # trace les ligne x * intervalle interquqrtile...

boxplot(pollution$pm25, col = "blue")
abline(h=12)
?abline()


# Histogram
colors()
hist(pollution$pm25, col = "paleturquoise3")
rug(pollution$pm25) # plot all the points
abline(v=12, lwd =4)
abline(v=median(pollution$pm25), col = "magenta" ,lwd =4)

# Barplot
class(pollution$region)
table(pollution$region) # to summarize categorical variable
class(table(pollution$region))
barplot(table(pollution$region), col = "wheat2", main = "Number of countries in each region")
# sorte d'histogramme de variable categorielles...
?barplot()


# ---------------------------------------------------------
# 2 Dimension
# ---------------------------------------------------------
par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1)) #  initialiser
# Multiple Boxplot
boxplot(pm25 ~ region, data = pollution , col = "red") # region categorical
?boxplot()

pollution$longitude>-90
boxplot(pm25 ~ region, data = pollution , subset = pollution$longitude>-100,  col = "red") # region categorical - subset exclu une partie...
# Une variable continue, une variable categorielle (autant de boxplot que de val)

# Multiple Histogram
par(mfrow = c(2,1), mar = c(4,4,2,1)) # ??? comment initialiser
hist(subset(pollution, region == "east")$pm25, col = "green")
hist(subset(pollution, region == "west")$pm25, col = "green")
# Une variable continue, une variable categorielle avec nb connu et faible de val...

# Scatterplot
with(pollution, plot(latitude, pm25))
abline(h=12, lwd =2, lty = 2)

with(pollution, plot(longitude, pm25))
abline(h=12, lwd =2, lty = 2)

par(mfrow = c(1,1), mar = c(4,4,2,1)) # ??? comment initialiser
with(pollution, plot(latitude, pm25, col = region))
abline(h=12, lwd =2, lty = 2)



with(pollution, plot(longitude, region))
abline(v=-100, lwd =2, lty = 2, col = "red")
# separation des r??gion ?? longitude de -100 approx..

# Multiple Scatterplots
par(mfrow = c(1,2), mar = c(5,4,2,1)) # ??? comment initialiser
with(subset(pollution, region == "west"), plot(latitude, pm25, main = "West"))
with(subset(pollution, region == "east"), plot(latitude, pm25, main = "East"))


# R Graph Gallery : http://rgraphgallery.blogspot.com.es
# R bloggers : http://www.r-bloggers.com
par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1)) #  initialiser

X = seq(1, 100, 5)
Y = seq (1, 100, 5)
Z = rnorm (length (X), 10, 2)
data1 <- data.frame (X, Y, Z)
data2 <- data.frame (X, Y, Z1 = Z - 5)
data3 <- data.frame (X, Y, Z1 = Z - 3)

require(scatterplot3d)
s3d <- scatterplot3d(data1, color = "blue", pch = 19, xlim=NULL, ylim=NULL, zlim= c(0, 20))
s3d$points3d(data2, col = "red", pch = 18)
s3d$points3d(data3, col = "green4", pch = 17)


# ---------------------------------------------------------
# Plotting Systems in R 
# ---------------------------------------------------------

# 1 - Base plotting system : "artist's palette" model

# Functions to generate the plot ... and f to annotate the plot
# ! can't go back

par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1)) #  initialiser

library(datasets)
data(cars)
str(cars)
with(cars, plot(speed,dist))

?plot()
?with() # n Expression in a Data Environment

# 2 - The Lattice System : Entire plot specified by 1 funct; conditioning

# Plots created with a single function
# Plots created with a single function call (xyplot, bwplot, ...)
# + conditioning , - difficult to anotate, ...

library(lattice)
state <- data.frame(state.x77, region = state.region)
xyplot(Life.Exp ~ Income | region, data = state, layout = c(4,1))

# 3 - The ggplot2 : Mixes elements of Base and Lattice


library(ggplot2)
data(mpg)
qplot(displ, hwy, data = mpg)

# ---------------------------------------------------------
# Base Plotting Systems in R 
# ---------------------------------------------------------

# Screen, fiule?
# How the plot will be used ? tmp screen, web, paper, pres
# resize dynamically? vector format vs bitmap format

# Quel system ? : base, lattice, ggplot2 (ne peuvent pas etre melanges)

# Base : 2 D, 2 phases: initialize, Annotating

# plot(x,y), hist(x) ... launch graph device... draw in that device

par(mfrow = c(1,1), mar = c(5.1,4.1,4.1,2.1)) #  initialiser

library(datasets)
str(airquality)
hist(airquality$Ozone)
with(airquality, plot(Wind, Ozone))
airquality <- transform(airquality, Month = factor(Month))
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)" )
boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)" ,col = "steelblue")


boxplot(Ozone ~ Month, airquality, xlab = "Month", ylab = "Ozone (ppb)" ,col = "steelblue", pch = 19)
# pch : plotting symbol (default open circle)
# lty : line type  (default solid)
# lwd : line width specified as an interger multiple
# col : color spec as a number , string, or hex code; colors() gives a vector of clolors by names
colors()
# xlab : character string for x-axis
# ylab : character string for y-axis


# par() function is used to specify global graphics param that affect all plots in a R session
# can be overridden for a spec graph with spec function call...

# las : orientation of axis label
# bg :  background color
# mar : margin size
# oma : outer margin size
# mfrow: number of plots per row, column (plots are filled row-wise)
# cfrow: number of plots per row, column (plots are filled column-wise)

# See default values :
par("lty")
par("col")
par("pch")
par("lwd")
par("xlab")
par("las")
par("bg")
par("mar")  # bottom left top righ
par("oma")
par("mfrow")
par("cfrow")

# Base Plotting Functions : 
# plot(), 
# lines() vector x vector y connects dots
# points(), 
# text(), pos x,y
# title(), 
# mtext(), 
# axis()

library(datasets)

with(airquality, plot(Wind, Ozone))
title(main = "Ozone and Wind in New York City")
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind ....NY", type = "n")) # set up sans plotter
with(subset(airquality, Month == 5), points(Wind, Ozone, col = "blue"))
with(subset(airquality, Month != 5), points(Wind, Ozone, col = "red"))
legend("topright", pch = 1, col = c("blue","red"), legend = c("May","Other months"))

with(airquality, plot(Wind, Ozone, main = "Ozone and Wind in NY", pch = 20)) 
model <- lm(Ozone ~ Wind, airquality)
abline(model, lwd = 2)

with(cars, plot(dist,speed))
model2 <- lm(speed ~ dist, cars)
abline(model2, lwd = 2, lty = 2, col = "red")

par(mfrow = c(1,2))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and sollar radiation")
})

par(mfrow = c(1,3), mar = c(4,4,2,1), oma = c(0,0,2,0))
with(airquality, {
  plot(Wind, Ozone, main = "Ozone and Wind")
  plot(Solar.R, Ozone, main = "Ozone and sollar radiation")
  plot(Temp, Ozone, main = "Ozone and Temp")
  mtext("xxx in NY", outer = TRUE)
})

# Demonstration

par(mfrow = c(1,1))

x <- rnorm(100)
hist(x)
y <- rnorm(100)
plot(x,y)
par("mar")

par(mar = c(2,2,0,0))
plot(x,y)
par(mar = c(5,4,2,2))
plot(x,y)

plot(x,y, pch = 20)

plot(x,y, pch = 19)
plot(x,y, pch = 2)
plot(x,y, pch = 3)
plot(x,y, pch = 4)

# Appendre les param : 
example(points)



x <- rnorm(100)
y <- rnorm(100)
plot(x,y, pch = 24, col = "red", bg = "blue") # Boundary and fill param col & bg
title("Scatterplot")
text(-2,-2, "label")
legend("topleft", legend = "DATA")
legend("topright", legend = "DATA", pch = 20)
fit <- lm(y ~ x )
abline(fit, col = "red", lty = 2)
abline(fit, lwd = 3, col = "red", lty = 2)
abline(fit, lwd = 3, col = "blue")
plot(x, y, xlab = "Weight", ylab = "Height", main = "TITRE", pch =20)
legend("topright", legend = "DATA LEGEND", pch = 20)
fit <- lm(y ~ x )
abline(fit, lwd = 3, col = "red")
z <- rpois(100,2)
par(mfrow = c(2,1))
plot(x,y,pch = 20)
plot(x,z ,pch = 19)
par("mar")
par(mar = c(2,2,1,1))
plot(x,y,pch = 20)
plot(x,z ,pch = 19)

par(mfrow = c(1,2))
plot(x,y,pch = 20)
plot(x,z ,pch = 19)

par(mfrow = c(2,3))
plot(x,y,pch = 20)
plot(x,z ,pch = 19)
plot(x,z ,pch = 14)
plot(x,z ,pch = 1)
plot(x,z ,pch = 17)
plot(x,z ,pch = 12)

par(mfcol = c(2,3)) # change l'ordre d'affichage par col...
plot(x,y,pch = 20)
plot(x,z ,pch = 19)
plot(x,z ,pch = 14)
plot(x,z ,pch = 1)
plot(x,z ,pch = 17)
plot(x,z ,pch = 12)



# Points

par(mfcol = c(1,1))
x <- rnorm(100)
y <- x + rnorm(100)
g <- gl(2,50)
g <- gl(2,50, labels = c("Male","Female"))
g
str(g)
plot(x,y)
plot(x,y, type = "n")
points(x[g == "Male"], y[g == "Male"], col = "green")
points(x[g == "Female"], y[g=="Female"], col = "blue", pch = 19)


# ----------------------------------------------------------
# Graphics Devices in R
# ----------------------------------------------------------

# Window
# PDF file, PNG or JPEG file
# SVG

# on Mac quartz()
?Devices

library(datasets)
str(faithful)
with(faithful, plot(eruptions, waiting))
title(main = " Old xxxxx")

pdf(file = "myplot.pdf")  ## Open PDF device; create myplot.pdf in wd
with(faithful, plot(eruptions, waiting))
title(main = " Old xxxxx")
dev.off() ## Close the PDF file devioce

png(file = "myplot.png")  ## Open PDF device; create myplot.pdf in wd
with(faithful, plot(eruptions, waiting))
title(main = " Old xxxxx")
dev.off() ## Close the png file devioce


# Vector formats: 
# pdf
# svg
# win.metafile
# postscript

# Bitmapformat
# png : portable network graphics
# jpeg : goog for photo, natural scenes
# tiff
# bmp

# Multiple Open Graphics Devices
dev.cur()
quartz()
dev.cur() # active device
# dev.set(<interger>) >2..
quartz()
quartz()
quartz()
quartz()
i<- dev.cur()
i
dev.set(6)
with(faithful, plot(eruptions, waiting))
title(main = " Old xx6xxx")


dev.set(3)
with(faithful, plot(eruptions, waiting))
title(main = " Old xxx3xx")

dev.set(6)
dev.off()

dev.set(4)
with(faithful, plot(eruptions, waiting))
title(main = " Old xx4xx4x")
dev.off()
dev.off()
dev.off()
dev.off()

# Copying Plots

# dev.copy
# dev.copy2pdf copy a plot to a PDF file

library(datasets)
with(faithful, plot(eruptions, waiting))
title(main = " Old xxxxx")
dev.copy(png, file = "myplot_with_dev_copy.png")
dev.off()

# Noter que les deux fichiers ne sont pas exactement l??es m??mes.... copie est moi pure...

# ----------------------------------------------------------
# Week 2 
# ----------------------------------------------------------

# ----------------------------------------------------------
# Lattice usefull for high dimenstional data / many plots
# xyplot, bwplot, levelplot & con, histogram, stripplot, dotplot, splom
# single function call to plot and annotation
# ----------------------------------------------------------

library(lattice)

# xyplot(y ~ x | f * g , data) formula notation : axe y ~ axe x | conditioning variables
# 2 eme arg data frame

library(datasets)
str(airquality)
summary(airquality)

xyplot(Ozone ~ Wind, data = airquality)
class(xyplot(Ozone ~ Wind, data = airquality))
# Objet de class "trellis" auto printe

o <- xyplot(Ozone ~ Wind, data = airquality)
print(o)
# Conversion du mois en valiable cat??gorielle (factor)
airquality <- transform(airquality, Month = factor(Month))
xyplot(Ozone ~ Wind | Month, data = airquality, layout = c(5,1))
# une seule ligne au lieu de plusieurs en Basis R...

# en fait objet of class trellis puis print graphicdevice
# auto printing....
p <- xyplot(Ozone ~ Wind, data = airquality)
print(p)

# Lattice Panel Functions
set.seed(10)
x <- rnorm(100)
f <- rep(0:1, each = 50)
f
y <- x + f - f * x + rnorm(100, sd = 0.5)
f <- factor(f, labels = c("group 1", "group 2"))
f
xyplot ( y ~ x | f, layout = c(2,1))

xyplot ( y ~ x | f, panel = function(x,y,...) {
  panel.xyplot(x,y,...)
  panel.abline(h = median(y), lty = 2)
})

xyplot ( y ~ x | f, panel = function(x,y,...) {
  panel.xyplot(x,y,...)
  panel.lmline(x,y, clo = 2)
})

?xyplot()

# Many panel possible... ex 150 Baltimor allergenes...

#----------------------------------------------------
# ggplot2
#----------------------------------------------------

Grammar of Graphics by Leland Wilkinson

library(ggplot2)

# data doivent venir d'un data.frame
# qplot() / ggplot
?mpg
str(mpg)

qplot(displ,hwy, data = mpg)

# add factor couleur
qplot(displ,hwy, data = mpg, color = drv)

# Add statistic
qplot(displ,hwy, data = mpg, geom = c("point","smooth"))

# Histogram avec 1 seule var
qplot(hwy, data = mpg)
qplot(hwy, data = mpg, fill = drv)

# Facets (equiv aux panels de lattice )
qplot(displ,hwy, data = mpg, facets =.~drv )

qplot(hwy, data = mpg, facets =.~drv , binwidth = 2)
qplot(hwy, data = mpg, facets =drv~. , binwidth = 2)
qplot(hwy, data = mpg, facets =drv~fl , binwidth = 2)
qplot(hwy, data = mpg, facets =drv~fl , binwidth = 2,color = class )
qplot(hwy, displ, data = mpg, facets =drv~. , binwidth = 2,color = class )


# str(maacs) not available
# qplot(log(eno), data=maacs)
# qplot(log(eno), data=maacs, fill = mopos)
# qplot(log(eno), data=maacs, geom = "density")
# qplot(log(eno), data=maacs, geom = "density", color = mopos)

# qplot(log(pm25),log(eno), data=maacs)
# qplot(log(pm25),log(eno), data=maacs, shape = mopos)
# qplot(log(pm25), log(eno), data=maacs, color = mopos)
# qplot(log(pm25), log(eno), data=maacs, color = mopos, geom = c("point","smooth"),method="lm")
# qplot(log(pm25), log(eno), data=maacs, geom = c("point","smooth"),method="lm",factes=.~mopo)

# tricky to modify... customization

# qplot(logpm25, NocturnalSympt, data=maacs, geom = c("point","smooth"),method="lm",factes=.~bmicat)


# data frame, 
# aesthetic mapping : how data are mapped (color size)
# geoms : points, lines , shapes
# facets : for conditional plots
# stats : stat transformation
# scales
# coord syst


# Piece by piece,,, unlike lattice
# in layers : data, overlay a summary, metadata and annotation

# g <- ggplot(maacs, aes(logpm25, Noct...))

# print(g)
# summary(g) ggplot objects

str(mpg)
summary(mpg)
g <- ggplot(mpg, aes(displ,hwy))
print(g) # pas assez d'info...
p <- g + geom_point()
print(p)

g+geom_point()

g + geom_point() + geom_smooth()
g + geom_point() + geom_smooth(method = "lm")
g + geom_point() + geom_smooth(method = "lm", se = FALSE)
g + geom_point() + facet_grid(.~drv) + geom_smooth(method = "lm")

xlab("aaa")
ylab()
labs()
ggtitle()
theme()
# two standard theme
theme_gray()
themebw()

g + geom_point(color ="steelblue",size =4, alpha = 1/2) # aplpha tranparency
g + geom_point(aes(color =drv),size =4, alpha = 1/2)
g + geom_point(aes(color =drv, size =cyl), alpha = 1/2)

g + geom_point(aes(color =drv),size =4, alpha = 1/2) + labs(title = "TITRE") + labs(x = expression("log "*PM[222]), y = "titre axe y")
g + geom_point(aes(color =drv),size =4, alpha = 1/2) + labs(title = "TITRE") + labs(title = "ZATRExxx")+ labs(x = expression("log "*PM), y = "titre axe y")

g + geom_point(aes(color =drv), size =4, alpha = 1/2) + geom_smooth(size = 2, linetype = 2, method = "lm", se = FALSE)
g + geom_point(aes(color =drv), size =4, alpha = 1/2) + geom_smooth(size = 2, linetype = 3, method = "lm", se = FALSE)
g + geom_point(aes(color =drv, size =cyl), alpha = 1/2) + geom_smooth(size = 2, linetype = 2, method = "lm", se = FALSE)
# Change the overall theme
g + geom_point(aes(color =drv))+ theme_bw(base_family = "Times")

testdata <- data.frame(x = 1:100, y=rnorm(100))
testdata[50,2] <- 100 ## outlier
plot(testdata$x,testdata$y, type = "l")
plot(testdata$x,testdata$y, type = "l", ylim = c(-3,3))
g <- ggplot(testdata, aes(x = x, y = y))
g + geom_line()
g + geom_line() + ylim(-3,3) # les donnees en dehors sont sorties... subset
g + geom_line() + coord_cartesian(ylim = c(-3,3)) # idem plot

# cut() decoupe une variable continuue ne categories

## Calculate the deciles
cutpoints <- quantile(mpg$cty, seq(0,1, length = 4), na.rm = TRUE)
cutpoints

## cut the data
mpg$cty2 <- cut(mpg$cty, cutpoints)
str(mpg)
levels(mpg$cty2)


## Setup ggplot with data frame
g <- ggplot(mpg, aes(displ,hwy))

## Add layers
# g + geom_point(alpha = 1/3)  + geom_smooth(method = "lm", se = FALSE, col="steelblue") + theme_bw(base_family = "Avenir", base_size = 10) +labs(x = expression("log " * PM[2.5])) +labs(y = "titre Y") +labs(title = "TITRE PRINCIPAL") + facet_wrap(cty2 ~ fl, nrow = 15, ncol = 3 )


g <- g + geom_point(alpha = 1/3)  + geom_smooth(method = "lm", se = FALSE, col="steelblue") 
g<- g + theme_bw(base_family = "Avenir", base_size = 10) +labs(x = expression("log " * PM[2.5])) +labs(y = "titre Y") +labs(title = "TITRE PRINCIPAL") 
g <- g + facet_wrap(cty2 ~ fl, nrow = 15, ncol = 3 )
g

# ----------------------------------------------------------
# Week 2 : Quizz
# ----------------------------------------------------------

# ----------------------------------------------------------
# 1 Under the lattice graphics system, what do the primary 
# plotting functions like xyplot() and bwplot() return?
# ----------------------------------------------------------

library(lattice)
library(datasets)

class(xyplot(Ozone ~ Wind, data = airquality))
o <- xyplot(Ozone ~ Wind, data = airquality)
print(o)
xyplot(Ozone ~ Wind, data = airquality)

# ----------------------------------------------------------
# 2 What is produced by the following code?
# library(nlme)
# library(lattice)
# xyplot(weight ~ Time | Diet, BodyWeight)
# ----------------------------------------------------------

library(nlme)
library(lattice)
str(BodyWeight)
xyplot(weight ~ Time | Diet, BodyWeight)

# ----------------------------------------------------------
# Question 3 
# Annotation of plots in any plotting system involves 
# adding points, lines, or text to the plot, 
# in addition to customizing axis labels or adding titles. 
# Different plotting systems have different sets of functions 
# for annotating plots in this way. Which of the following functions 
# can be used to annotate the panels in a multi-panel lattice plot?
# ----------------------------------------------------------

???????
?xyplot

xyplot(weight ~ Time | Diet, BodyWeight)
llines(10,  type = "l", col="red")


xyplot(weight ~ Time | Diet, BodyWeight, text("aaa"), panel = function(x,y,...) {
  panel.xyplot(x,y,...)
  panel.text(1,1,"toto")
})


?xyplot()
# ----------------------------------------------------------
# Question 4 
# The following code does NOT result in a plot appearing on the screen device.
# Which of the following is an explanation for why no plot appears?
# ----------------------------------------------------------
library(lattice)
library(datasets)
data(airquality)
p <- xyplot(Ozone ~ Wind | factor(Month), data = airquality)
class(p)
print(p)


#----------------------------------------------------------
# Question 5 
# In the lattice system, which of the following functions 
# can be used to finely control the appearance of all lattice plots?
# ----------------------------------------------------------

???????

#----------------------------------------------------------
# Question 6 
# What is ggplot2 an implementation of?
#----------------------------------------------------------

# the Grammar of Graphics developed by Leland Wilkinson

#----------------------------------------------------------
# Question 7 
# Load the `airquality' dataset form the datasets package in R.

library(datasets)
data(airquality)

# I am interested in examining how the relationship between 
# ozone and wind speed varies across each month. 
# What would be the appropriate code to visualize that using ggplot2?
#----------------------------------------------------------

qplot(Wind, Ozone, data = airquality, geom = "smooth")

airquality = transform(airquality, Month = factor(Month))
qplot(Wind, Ozone, data = airquality, facets = . ~ Month)

qplot(Wind, Ozone, data = airquality)

qplot(Wind, Ozone, data = airquality, facets = . ~ factor(Month))

#----------------------------------------------------------
# Question 8
# What is a geom in the ggplot2 system?

# a plotting object like point, line, or other shape
#----------------------------------------------------------

#----------------------------------------------------------
# Question 9
# When I run the following code I get an error:
library(ggplot2)
g <- ggplot(movies, aes(votes, rating))
print(g)
# ggplot does not yet know what type of layer to add to the plot.
#----------------------------------------------------------
g+geom_point()

#----------------------------------------------------------
# Question 10
# The following code creates a scatterplot of 'votes' and 'rating' 
# from the movies dataset in the ggplot2 package. 
# After loading the ggplot2 package with the library() function, I can run
qplot(votes, rating, data = movies)
# How can I modify the the code above to add a smoother to the scatterplot?
#----------------------------------------------------------
qplot(votes, rating, data = movies, smooth = "loess")

qplot(votes, rating, data = movies) + geom_smooth() # YES

qplot(votes, rating, data = movies, panel = panel.loess)
qplot(votes, rating, data = movies) + stats_smooth("loess")


# ----------------------------------------------------------
# Week 3 - clustering
# ----------------------------------------------------------

# Notion de distance
# Methode de groupage
# Visualisation et interpretation ...

# ----------------------------------------------------------
# Hierarchical Clustering
# ----------------------------------------------------------
# Pas mal pour voir dans les high dim dadates...

# Approach agglomerative...
# Algo : 
#       tant qu'il reste plusieurs groupe/elements
#               trouver les deux groupe/elements les plus proches entre eux au sens de la distance choisie
#               remplacer grouper des les deux groupes/elements 


#       Pour i=1 ?? individus.longueur Faire
#               classes.ajouter(nouvelle classe(individu[i]));
#       Fin Pour
#       Tant Que classes.longueur > nbClasses Faire
#               // Calcul des dissimilarit??s entre classes dans une matrice triangulaire sup??rieure
#               // suppose au moin la propriete de symetrie poure dissim
#               matDissim = nouvelle matrice(classes.longueur,classes.longueur);
#               Pour i=1 ?? classes.longueur Faire
#                       Pour j=i+1 ?? classes.longueur Faire
#                               matDissim[i][j] = dissim(classes[i],classes[j]);
#                       Fin Pour
#               Fin Pour
#               // Recherche du minimum des dissimilarit??s
#               Soit (i,j) tel que matDissim[i][j] = min(matDissim[k][l]) avec 1<=k<=classes.longueur et k+1<=l<=classes.longueur;
#               // au fait si plus de 2 ??? ok en fait ... sauf que l'arbre binaire depend du codage...
#               // Fusion de classes[i] et classes[j]
#               Pour tout element dans classes[j] Faire
#                       classes[i].ajouter(element);
#               Fin pour
#               supprimer(classes[j]);
#       Fin Tant Que


# Deux methodes : distance & merging approach

# Le resultat est un arbre binaire dont les noeuds sont les groupes et les feuilles les elements
# les noeuds ont un attribut quantitatif : la distance entre ses deux fils...

# En mathematiques, on appelle distance sur un ensemble E une application 
# d definie sur le produit E2 = EXE et a valeurs dans l'ensemble R+ des reels positifs,

# d :E X E -> R+ verifiant les proprietes suivantes :
#       - symetrie : qq A et B, de E d(A,B) = d(B,A)
#       - separation : qq A, B de ExE, d(A,B) = 0 <=> A = B
#       - sous-additivite / inegatite triangulaire : qq A,B,C de E d(A,C) <= d(A,B)+d(B,C)

# Sur un esp vect norme (E,N)
# N(y-x) est un distance c'est la distance canonique
# vu que une norme sur un K esp vect E ou K est numi d'une val abs
# est plus restrictif... avec homogeneite (N(a.V)=abs(a).N(V))

# Distance classique : 
#       Euclidienne
#       Manhatan
#       Minkovski ordre p (racine p iemme de la somme des val abs des diff de puiss p) ie g??neralisation Eucl & M
#               Appellee la p distance
#       Distance de Tchebychev limite p->inf de d de Minkovski

# Cas interessant est la distance ultrametrique
# Inegalite triangulaire remplacee par d(a,c) <= max(d(a,b),d(b,c))
#       tous les triengles ont isoceles, ... 
#       les boules de rayon r donn?? forme un partition...
#       on peut donc faire du clustering en faisant varier r de 0 a max,...

set.seed(1234)

par(mfrow = c(1, 1), mar = c(4, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

dataFrame <- data.frame(x = x, y = y)
dist(dataFrame) # Matrice des distance (par defaut euclidienne) entres les points 
?dist()
dist(dataFrame, method = "manhattan")   # L1 distance
dist(dataFrame, method = "maximum")     # Tchebychev
dist(dataFrame, method = "binary")      # coord trans en 0 ou 1 puis prop de 1 sur la diff??rence...
dist(dataFrame, method = "canberra")      # canberra
dist(dataFrame, method = "minkowski", p = 5)

dist <- dist(dataFrame, method = "minkowski", p = 5)
class(dist)

dist(dataFrame, method = "maximum", diag = TRUE) 
dist(dataFrame, method = "maximum", diag = TRUE, upper = TRUE) 


# -----------------------------------------------------------------

?text()

x <- rnorm(100, mean = rep(1:4, each = 25), sd = 0.1)
y <- rnorm(100, mean = rep(c(1, 2, 1,2), each = 25), sd = 0.1)
y <- y[sample(1:100)]
plot(x, y, col = "blue", pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5)

par(mfrow = c(1, 2), mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))

#--------------------------------------------------
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)

par(mfrow = c(1, 2))
par(mar = c(5.1, 4.1, 4.1, 2.1))
plot(x, y, col = "blue", pch = 19, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:12), ces=1)

dataFrame <- data.frame(x = x, y = y)
dataFrame
distxy <- dist(dataFrame) # euclidienne par defaut
distxy
hClustering <- hclust(distxy)
plot(hClustering) # dendrogram


class(hClustering)
attributes(hClustering)
hClustering
hClustering$merge
hClustering$height
hClustering$order
hClustering$labels
hClustering$method
hClustering$call
hClustering$dist.method

?hclust()
# method : the agglomeration method to be used
# method = "ward.D", "ward.D2", "single", "complete", 
# "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC)


# A number of different clustering methods are provided. 
# Ward's minimum variance method aims at finding compact, spherical clusters. 
# (minimise l'intertie intraclasse / maximise l'intertie interclasse)
# The complete linkage method finds similar clusters. (saut maximun : dissim(C1,C2)=max dissim(x,y))
# The single linkage method (which is closely related to the minimal spanning tree) adopts a ???friends of friends??? clustering strategy. 
# dissim(C1,C2)= min  dissim(x,y)
# The other methods can be regarded as aiming for clusters with characteristics somewhere between the single and complete link methods. 
# Note however, that methods "median" and "centroid" are not leading to a monotone distance measure, 
# or equivalently the resulting dendrograms can have so called inversions or reversals which are hard to interpret, 
# but note the trichotomies in Legendre and Legendre (2012).




par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0))

plot(x, y, col = "blue", pch = 19, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:12), cex=1)

par(mar = c(5.1, 4.1, 4.1, 2.1))

dataFrame <- data.frame(x = x, y = y)
dataFrame
distxy <- dist(dataFrame) # euclidienne par defaut
distxy
H1 <- hclust(distxy, method = "ward.D")
plot(H1) # dendrogram
H2 <- hclust(distxy, method = "complete")
plot(H2) # dendrogram
H3 <- hclust(distxy, method = "single")
plot(H3) 

#---------------autre distance------------------------

par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0))

plot(x, y, col = "blue", pch = 19, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:12), cex=1)

par(mar = c(5.1, 4.1, 4.1, 2.1))

dataFrame <- data.frame(x = x, y = y)
dataFrame
distxy <- dist(dataFrame,method = "maximum") # L1
distxy
H1 <- hclust(distxy, method = "ward.D")
plot(H1) # dendrogram
H2 <- hclust(distxy, method = "complete")
plot(H2) # dendrogram
H3 <- hclust(distxy, method = "single")
plot(H3) 


#---------------------------------------------------

# ---------------plus de points 100--------------------------------------------------
set.seed(1234)
x <- rnorm(100, mean = rep(1:4, each = 25), sd = 0.35)
y <- rnorm(100, mean = rep(c(1, 2, 1,2), each = 25), sd = 0.35)

par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0))
plot(x, y, col = "blue", pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5)
par(mar = c(5.1, 4.1, 4.1, 2.1))

dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame,method = "euclidean") # L1
H1 <- hclust(distxy, method = "ward.D")
plot(H1) # dendrogram
H2 <- hclust(distxy, method = "complete")
plot(H2) # dendrogram
H3 <- hclust(distxy, method = "single")
plot(H3) 



H1$order
H2$order
H3$order

colour1 <- cutree(H1, k=4)
colour2 <- cutree(H2, k=4)
colour3 <- cutree(H3, k=4)
colour3
par(mar = c(0, 0, 0, 0))
plot(x, y, col = "blue", pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5)
plot(x, y, col = "blue", pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5, col = colour1)
plot(x, y, col = "blue", pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5, col = colour2)
plot(x, y, col = "blue", pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5, col = colour3)


#-----------------pour un nombre de clustyer donn??....colorier....



myplclust <- function(hclust, lab = hclust$labels, lab.col = rep(1, length(hclust$labels)), 
                      hang = 0.1, ...) {
  ## modifiction of plclust for plotting hclust objects *in colour*!  Copyright
  ## Eva KF Chan 2009 Arguments: hclust: hclust object lab: a character vector
  ## of labels of the leaves of the tree lab.col: colour for the labels;
  ## NA=default device foreground colour hang: as in hclust & plclust Side
  ## effect: A display of hierarchical cluster with coloured leaf labels.
  y <- rep(hclust$height, 2)
  x <- as.numeric(hclust$merge)
  y <- y[which(x < 0)]
  x <- x[which(x < 0)]
  x <- abs(x)
  y <- y[order(x)]
  x <- x[order(x)]
  plot(hclust, labels = FALSE, hang = hang, ...)
  text(x = x, y = y[hclust$order] - (max(hclust$height) * hang), labels = lab[hclust$order], 
       col = lab.col[hclust$order], srt = 90, adj = c(1, 0.5), xpd = NA, ...)
}



par(mfrow = c(2, 2))
par(mar = c(0, 0, 0, 0))
plot(x, y, col = "blue", pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5)
par(mar = c(5.1, 4.1, 4.1, 2.1))

myplclust(H1, lab= H1$order, lab.col = rep(1:4, each = 25))
myplclust(H2, lab= H2$order, lab.col = rep(1:4, each = 25))
myplclust(H3, lab= H3$order, lab.col = rep(1:4, each = 25))


# http://gallery.r-enthusiasts.com/RGraphGallery.php?graph=79

# How to merge points together....
# average...
# Complete linkage : prendre la plus grande des distance des couples des 2 clusters...
# la methode average donnerait la distance entre les deux baricentres
set.seed(1234)

par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)


dataFrame <- data.frame(x = x, y = y)
distxy <- dist(dataFrame)
hClustering <- hclust(distxy)
myplclust(hClustering, lab = rep(1:3, each = 4), lab.col = rep(1:3, each = 4))
myplclust(hClustering, lab = hClustering$order, lab.col = rep(1:3, each = 4))

dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
heatmap(dataMatrix)

x <- rnorm(100, mean = rep(1:4, each = 25), sd = 0.2)
y <- rnorm(100, mean = rep(c(1, 2, 1,2), each = 25), sd = 0.1)
y <- y[sample(1:100)]
plot(x, y, col = "blue", pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5)
dataFrame <- data.frame(x = x, y = y)
set.seed(143)
dataMatrix <- as.matrix(dataFrame)[sample(1:100), ]
heatmap(dataMatrix)
?heatmap
# ----------------------------------------------------------
# heatmap interessant pour voir rapidement des gros dataset X avec 
# beaucoup de lignes et/ou de colonnes
# ----------------------------------------------------------

# Attention a la sensibilite au scaling et aux points extremes...
# tester en changeant la distance metrique ou merging strat

# ----------------------------------------------------------
# K-Means Clustering
# ----------------------------------------------------------

?kmeans

# partioning approach
# param : number of cluster, initials centroids, distance... 
# Algo 
#   Affecter les ??l??ments aux centroids (au sens de la distance choisie)
#   recalculer les centroid (sorte de bericentre...)
# Arret apres un certain nb d'it??ration? quand les cluster ne changent plus?
set.seed(1234)
par(mar = c(0, 0, 0, 0))
x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))



dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 3)
names(kmeansObj)


kmeansObj$cluster # vecteur donnant le cluster du point d'indice i

kmeansObj$centers # coord des centres

?kmeans()

par(mar = rep(0.2, 4))
plot(x, y, col = kmeansObj$cluster, pch = 19, cex = 2)
points(kmeansObj$centers, col = 1:3, pch = 3, cex = 3, lwd = 3)


#----------------------------------------------------------
par(mfrow = c(1, 1), mar = c(5.1, 4.1, 4.1, 2.1))
x <- rnorm(100, mean = rep(1:4, each = 25), sd = 0.2)
y <- rnorm(100, mean = rep(c(1, 2, 1,2), each = 25), sd = 0.2)
plot(x, y, col = "blue", pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5)

dataFrame <- data.frame(x, y)
kmeansObj <- kmeans(dataFrame, centers = 4)
plot(x, y, col = kmeansObj$cluster, pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5)

kmeansObj <- kmeans(dataFrame, centers = 3)
plot(x, y, col = kmeansObj$cluster, pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5)

kmeansObj <- kmeans(dataFrame, centers = 2)
plot(x, y, col = kmeansObj$cluster, pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5)


kmeansObj <- kmeans(dataFrame, centers = 5)
plot(x, y, col = kmeansObj$cluster, pch = 18, cex = 1)
text(x + 0.05, y + 0.05, labels = as.character(1:100), cex = 0.5)


x <- rnorm(12, mean = rep(1:3, each = 4), sd = 0.2)
y <- rnorm(12, mean = rep(c(1, 2, 1), each = 4), sd = 0.2)
plot(x, y, col = "blue", pch = 19, cex = 2)
text(x + 0.05, y + 0.05, labels = as.character(1:12))
dataFrame <- data.frame(x, y)

set.seed(1234)
dataMatrix <- as.matrix(dataFrame)[sample(1:12), ]
kmeansObj2 <- kmeans(dataMatrix, centers = 3)
par(mfrow = c(1, 2), mar = c(2, 4, 0.1, 0.1))
image(t(dataMatrix)[, nrow(dataMatrix):1], yaxt = "n")
image(t(dataMatrix)[, order(kmeansObj2$cluster)], yaxt = "n")

# det Nb of clusters
# https://en.wikipedia.org/wiki/Determining_the_number_of_clusters_in_a_data_set


#----------------------------------------------------------------------------
# Dimension reduction
# Principal Component analysis
# Singular value decomposition
#----------------------------------------------------------------------------
par(mar = rep(0.2, 4),mfrow = c(1, 1))
set.seed(12345)
par(mar = rep(0.2, 4))
dataMatrix <- matrix(rnorm(400), nrow = 40)
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
# image(1:10, 1:40, t(dataMatrix))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1], col = rainbow(10))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1], col = terrain.colors(5, alpha = 1))

dim(dataMatrix)
dim(t(dataMatrix))

?t()
?image()
M <- matrix(rep(0,400),nrow = 10 )
image(1:10, 1:40, M)
M <- matrix(rep(1,400),nrow = 10 )
image(1:10, 1:40, M)
M[3,] <- 0
image(1:10, 1:40, M)
M <- matrix(rep(sample(1:10),40),nrow = 10 )
image(1:10, 1:40, M)


image(1:10, 1:40, t(matrix(rep(10,400),nrow = 40 )))

heatmap(dataMatrix)

heatmap(dataMatrix, col = rainbow(100))

set.seed(678910)
for (i in 1:40) {
  # flip a coin
  coinFlip <- rbinom(1, size = 1, prob = 0.5)
  # if coin is heads add a common pattern to that row
  if (coinFlip) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 3), each = 5)
  }
}

rep(c(0, 3), each = 5)

par(mar = rep(0.2, 4))
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])


par(mar = rep(0.2, 4))
heatmap(dataMatrix,col = rainbow(100))
heatmap(dataMatrix)

# clustering hiearchique sur les lignes....du data frame / matrix
hh <- hclust(dist(dataMatrix))
class(hh)
attributes(hh)
hh$order
hh

hh$merge
hh$height # les distance succecives....pour grouper ... 40-1
hh$order # les indices des noeuds dans l'ordre du rattachement
hh$labels
hh$method
hh$call
hh$dist.method

dataMatrixOrdered <- dataMatrix[hh$order, ]

par(mfrow = c(1, 3), mar = c(5.1, 4.1,4.1,2.1))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rowMeans(dataMatrixOrdered), 40:1, , xlab = "Row Mean", ylab = "Row", pch = 19)
plot(colMeans(dataMatrixOrdered), xlab = "Column", ylab = "Column Mean", pch = 19)

rowMeans(dataMatrixOrdered)
colMeans(dataMatrixOrdered)

# pourquoi les dis entre les points avec 3 serait elle moindre????


# multivariate X1, ....Xn so X1 = (X11,...,X1n)
# trouver une matrice de rang inf??rieur... epxliquant suff les donn??ees

# SVD
# Singuliar Value Decomposition
# X matrix column variable, rows obs
# X = U*D*t(V)
# U (left singular vecteurs) orthogonal
# V (right  singular vecteurs) orthogonal 
# D diagonal matrix (singular values)

# PCA Principal component analysis
# SVD a partir de Xnorm (chaque colonne normalis??e ie (Xci - mean (X(ci)) / sd(Xci))
# V est la PCA

svd1 <- svd(scale(dataMatrixOrdered))
class(svd1)
svd1
# liste avec d les 10 valeur propores, u matrice 40,10 et V matrice 10x10
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 1], 40:1, , xlab = "Row", ylab = "First left singular vector", 
     pch = 19)
plot(svd1$v[, 1], xlab = "Column", ylab = "First right singular vector", pch = 19)


image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd1$u[, 2], 40:1, , xlab = "Row", ylab = "2 left singular vector", 
     pch = 19)
plot(svd1$v[, 2], xlab = "Column", ylab = "2 right singular vector", pch = 19)



par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Prop. of variance explained", 
     pch = 19)

?scale() # pour centrer et r??duire les colonnes d'une matrice
svd1 <- svd(scale(dataMatrixOrdered))
pca1 <- prcomp(dataMatrixOrdered, scale = TRUE)

class(pca1)
pca1
svd1$v

pca1$rotation[, 1]
svd1$v[, 1]
identical(pca1$rotation[, 1],svd1$v[, 1])

plot(pca1$rotation[, 1], svd1$v[, 1], pch = 19, xlab = "Principal Component 1", 
     ylab = "Right Singular Vector 1")
abline(c(0, 1))


constantMatrix <- dataMatrixOrdered*0
for(i in 1:dim(dataMatrixOrdered)[1]){constantMatrix[i,] <- rep(c(0,1),each=5)}
constantMatrix

svd1 <- svd(constantMatrix)
svd1$v[, 1]
svd1$u[, 1]
svd1$d[1]
svd1$d[2]
round(svd1$d^2/sum(svd1$d^2),3)

par(mfrow=c(1,3))
image(t(constantMatrix)[,nrow(constantMatrix):1])
plot(svd1$d,xlab="Column",ylab="Singular value",pch=19)
plot(svd1$d^2/sum(svd1$d^2),xlab="Column",ylab="Prop. of variance explained",pch=19)


image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])

set.seed(678910)
for (i in 1:40) {
  # flip a coin
  coinFlip1 <- rbinom(1, size = 1, prob = 0.5)
  coinFlip2 <- rbinom(1, size = 1, prob = 0.5)
  # if coin is heads add a common pattern to that row
  if (coinFlip1) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), each = 5)
  }
  if (coinFlip2) {
    dataMatrix[i, ] <- dataMatrix[i, ] + rep(c(0, 5), 5)
  }
}
image(1:10, 1:40, t(dataMatrix)[, nrow(dataMatrix):1])
hh <- hclust(dist(dataMatrix))
dataMatrixOrdered <- dataMatrix[hh$order, ]

rep(c(0, 5), 5)
rep(c(0, 5), each = 5)

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(rep(c(0, 1), each = 5), pch = 19, xlab = "Column", ylab = "Pattern 1")
plot(rep(c(0, 1), 5), pch = 19, xlab = "Column", ylab = "Pattern 2")

svd2 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 3))
image(t(dataMatrixOrdered)[, nrow(dataMatrixOrdered):1])
plot(svd2$v[, 1], pch = 19, xlab = "Column", ylab = "First right singular vector")
plot(svd2$v[, 2], pch = 19, xlab = "Column", ylab = "Second right singular vector")


svd1 <- svd(scale(dataMatrixOrdered))
par(mfrow = c(1, 2))
plot(svd1$d, xlab = "Column", ylab = "Singular value", pch = 19)
plot(svd1$d^2/sum(svd1$d^2), xlab = "Column", ylab = "Percent of variance explained", 
     pch = 19)



dataMatrix2 <- dataMatrixOrdered
dim(dataMatrix2)
length(dataMatrix2)
## Randomly insert some missing data (sur 40 valeurs des 100 premieres...)
dataMatrix2[sample(1:100, size = 40, replace = FALSE)] <- NA
svd1 <- svd(scale(dataMatrix2))  ## Doesn't work!

sample(1:100, size = 40, replace = FALSE)


# Il faut doncfaire qq chose pour remplacer les missing values

#source("http://bioconductor.org/biocLite.R")
#biocLite()
#biocLite(c("impute"))
library(impute)  ## Available from http://bioconductor.org

image(1:10,1:40,t(dataMatrixOrdered))

dataMatrix2 <- dataMatrixOrdered
dataMatrix2[sample(1:100,size=40,replace=FALSE)] <- NA
sum(is.na(dataMatrix2[1:400]))
dataMatrix2 <- impute.knn(dataMatrix2)$data
sum(is.na(dataMatrix2[1:400]))
?impute.knn()
class(dataMatrix2)
# k-NN algorithm 

# In k-NN classification, 
# the output is a class membership. 
# An object is classified by a majority vote of its neighbors, 
# with the object being assigned to the class most common 
# among its k nearest neighbors (k is a positive integer, typically small). 
# If k = 1, then the object is simply assigned to the class of that single nearest neighbor.

# In k-NN regression, 
# the output is the property value for the object. 
# This value is the average of the values of its k nearest neighbors

# Ici on prend k(10 par defaut) k nearest neibours of the row
# mesur?? avec quelle distance ???
# par exemple avec k = 5 ... +- moyenne des 5


svd1 <- svd(scale(dataMatrixOrdered)); svd2 <- svd(scale(dataMatrix2))
par(mfrow=c(1,2)); plot(svd1$v[,1],pch=19); plot(svd2$v[,1],pch=19)


load("../data/face.rda")
class(faceData)
dim(faceData)
image(t(faceData)[, nrow(faceData):1])

svd1 <- svd(scale(faceData))
plot(svd1$d^2/sum(svd1$d^2), pch = 19, xlab = "Singular vector", ylab = "Variance explained")



svd1 <- svd(scale(faceData))
## Note that %*% is matrix multiplication

# Here svd1$d[1] is a constant
approx1 <- svd1$u[, 1] %*% t(svd1$v[, 1]) * svd1$d[1]

# In these examples we need to make the diagonal matrix out of d
approx5 <- svd1$u[, 1:5] %*% diag(svd1$d[1:5]) %*% t(svd1$v[, 1:5])
approx10 <- svd1$u[, 1:10] %*% diag(svd1$d[1:10]) %*% t(svd1$v[, 1:10])

par(mfrow = c(1, 4))
image(t(approx1)[, nrow(approx1):1], main = "(a)")
image(t(approx5)[, nrow(approx5):1], main = "(b)")
image(t(approx10)[, nrow(approx10):1], main = "(c)")
image(t(faceData)[, nrow(faceData):1], main = "(d)")  ## Original data

# Bruitage

load("../data/face.rda")
class(faceData)
dim(faceData)
image(t(faceData)[, nrow(faceData):1])


faceDataBruit10 <- faceData
faceDataBruit50 <- faceData
faceDataBruit80 <- faceData
faceDataBruit10[sample(1:1024, size = 100, replace = FALSE)] <- NA
faceDataBruit50[sample(1:1024, size = 500, replace = FALSE)] <- NA
faceDataBruit80[sample(1:1024, size = 800, replace = FALSE)] <- NA

image(t(faceData)[, nrow(faceData):1], main = "(origin)")
image(t(faceDataBruit10)[, nrow(faceDataBruit10):1], main = "(10%)")
image(t(faceDataBruit50)[, nrow(faceDataBruit10):1], main = "(50%)")
image(t(faceDataBruit80)[, nrow(faceDataBruit10):1], main = "(80%)")
?image()

# tentative de reconstruction avec k-NN
faceDataBruit10New <- impute.knn(faceDataBruit10)$data
faceDataBruit50New <- impute.knn(faceDataBruit50)$data
faceDataBruit80New <- impute.knn(faceDataBruit80)$data

image(t(faceData)[, nrow(faceData):1], main = "(origin)")
image(t(faceDataBruit10)[, nrow(faceDataBruit10):1], main = "(10%)")
image(t(faceDataBruit10New)[, nrow(faceDataBruit10New):1], main = "(New 10%)")

image(t(faceData)[, nrow(faceData):1], main = "(origin)")
image(t(faceDataBruit50)[, nrow(faceDataBruit50):1], main = "(50%)")
image(t(faceDataBruit50New)[, nrow(faceDataBruit50New):1], main = "(New 50%)")
svd50 <- svd(scale(faceDataBruit50New))
plot(svd50$d^2/sum(svd50$d^2), pch = 19, xlab = "Singular vector", ylab = "Variance explained")
approx50_6 <- svd50$u[, 1:6] %*% diag(svd50$d[1:6]) %*% t(svd50$v[, 1:6])
image(t(faceData)[, nrow(faceData):1], main = "(origin)")
image(t(faceDataBruit50)[, nrow(faceDataBruit50):1], main = "(50%)")
image(t(faceDataBruit50New)[, nrow(faceDataBruit50New):1], main = "(New 50%)")
image(t(faceDataBruit50New)[, nrow(faceDataBruit50New):1], main = "(Approx 6d New 50%)")


# Scale matters...
# Pattern mixed together
# Computation intensive....

# Alternatives : 
# Factor analysis / Independent component analysis / Latent semantic analysis

#----------------------------------------
# Working with Color in R Plots
#----------------------------------------
# default in R is bad
par(mfrow=c(1,2))
library(datasets)
data(volcano)
str(volcano)
class(volcano)
image(volcano, col = heat.colors(50))
image(volcano, col = topo.colors(50))



library(grDevices)
colors()
# function colorRamp() et colorRampPalette()



pal <- colorRamp(c("red", "blue"))
class(pal)

pal(0)
pal(1)
pal(0.5)
pal(seq(0, 1, len = 10))


pal <- colorRampPalette(c("red", "yellow"))
pal(2) # renvoie vecteur caractere de longueur 2 
# correspondant aux 2 couleurs..
# couleur codee en hexadecimal 2 digit pour Red 2 digit pour Green , 2 pour Bleu
# ex FF0000 est Red 255, Green 0 Blue 0
pal(10)


# trois type de palettes 
# Sequential / Diverging / Qualitative

library(RColorBrewer)
?brewer.pal()
brewer.pal.info

# Sequential 
cols <- brewer.pal(3, "BuGn")
cols
cols <- brewer.pal(5, "BuGn")
cols
cols <- brewer.pal(3, "BuGn")
cols

pal <- colorRampPalette(cols)
image(volcano, col = pal(20))

# Qualitative
cols <- brewer.pal(8, "Paired")
cols
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))
# debille

# divergeant
cols <- brewer.pal(8, "RdYlBu")
cols
pal <- colorRampPalette(cols)
image(volcano, col = pal(20))
# debille


x <- rnorm(10000)
y <- rnorm(10000)
plot(x,y)

smoothScatter(x,y)
?plot()

?rgb()
rgb(1,0,0)
# HEX string to be passed to plot, ...

rgb(1,0.1,0.8, 0.2) # dernier param est la non transparence de 0 a 1

plot(x,y,col = rgb(1,0,0, 0.1) ,pch = 19)
smoothScatter(x,y)

plot(x,y,col = rgb(0.8,0.3,0.8, 0.05) ,pch = 19)
smoothScatter(x,y)




