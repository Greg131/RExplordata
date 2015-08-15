# ----------------------------------------------------------
# R Exploratory Data Analysis
# ----------------------------------------------------------

setwd("/Users/GA/dropbox/Datascience/Universite_Johns-Hopkins/Exploration_analytique_de_donnees/RExplordata")
getwd() # Get working directory 

if (!file.exists("../data"))       {
  dir.create("../data")
}

pollution <- read.csv("../data/avgpm25.csv", colClasses = c("numeric","character","factor",
                                                           "numeric","numeric"))
head(pollution)
str(pollution)
summary(pollution)
# ----------------------------------------------------------
# Simple Summaries of the dada 
# 1 dim : summary, boxplot (boites a moustache), histogram, barplot
# 2 dim : multiple/overlayed 1-D plots, Scaterplots
# >2 dim : multiple/overlayed 2-D plots, add Color, size , shape
#         spinning plots, 3-D plots
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

# Boxplot (1 variables num??riques...)
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
