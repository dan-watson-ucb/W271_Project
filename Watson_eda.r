######################################################################
## Overall thoughts from EDA
##  Lot.Frontage may be important, may be same as lot size- didn't
##  review that variable here.  Basements seem to matter.  Maybe 
##  create an interaction term based on type...do we want to include
##  multiple basement types and multiple interactions for this?
##  Squarefootage seems to matter, do we have total vs upstairs/downstiars?
##  Pools and fences probably aren't important, we can expand upon this
##  if necessary, but doesn't seem worth going much further with these.
##
######################################################################




## Variables reviewed
## Lot Frontage, BsmtFinSF1, BsmtFinSF2, BSMT Unf SF, Total Bsmt SF
## 1st Flr SF, 2nd Flr SF, Low Qual Fin SF, Pool Area, House Style
## Roof Style, Roof Matl, Paved Drive, Pool QC, Fence

## Load libraries
library(car)
library(lmtest)
library(sandwich)

## Set WD
getwd()
setwd("Dropbox/Berkeley/W271/Lab3/W271_Project-master/")

df <- read.csv("AmesHousing_data_2010.csv")

############################################################
#Variable Analysis
###########################################################

## Lot.Frontage
## Linear feet of street connected to property

typeof(df$Lot.Frontage)
length(df$Lot.Frontage)
## No missing values

table(df$Lot.Frontage)
hist(df$Lot.Frontage)
## Not terribly skewed- slight R tail, could transform, 
## but probably not necessary
summary(df$Lot.Frontage)
## odd that NA comes up with 51 here, but NA values not in table.  
##Mean 68.79, median 70.  Range 21-152
sum(is.na(df$Lot.Frontage))
## Reviewing data there are NA values, should we use complete cases?
cc<-complete.cases(df)
df2<-df[cc, ]
## There are no complete cases at this point, seems that a lot
## of variables have no values- such as alley.  Can see if complete
## cases of our final variables makes sense to use.

## Plot relationship
plot(df$Lot.Frontage, df$SalePrice, main = "Sales Price Vs Lot.Frontage", col ="blue")
abline(lm(SalePrice~Lot.Frontage, data= df), col = "red")
## Does not seem to be a tight linear fit between the variables

## Simple linear model
ModelFrontage <- lm(SalePrice~Lot.Frontage, data= df)
summary(ModelFrontage)
## Statistically significant relationship
plot(ModelFrontage)
## QQ plot looks good until tail
## Seems like we have heteroskedasticity
coeftest(ModelFrontage, vcov= vcovHC)
## Appears significant, T value 4.3876, P 1.610e-05

############################################################

## BsmtFinSF1
## This tells us the SQFT of the basement type1
## Based on description perhaps we should do bsmt type * sqft
## As this variable by itself won't provide much context
typeof(df$BsmtFin.SF.1)
length(df$BsmtFin.SF.1)

hist(df$BsmtFin.SF.1)
## Note the huge number of 0s for basements
table(df$BsmtFin.SF.1)
## 92 properties have no basement, but concerned about values
## even through 104sqft- maybe a 10x10 room is considered a basement
## but what about 49sqft- 7x7, is that a basement?
## 24sqft?  Where is the line, should we even consider this variable?
summary(df$BsmtFin.SF.1)
plot(df$BsmtFin.SF.1, df$SalePrice, col="blue", main = "SalesPrice v BsmtFin.SF.1")
abline(lm(SalePrice~ BsmtFin.SF.1, data= df), col ="red")
## There seems to be some relationship, not tightly fitted
## I don't think it makes sense at this time to do an analysis
## of this as a linear model until we decide if we want to explore
## basements at all due to the limited number of responses, then
## we should create an interaction term with sqft of types

#############################################################

## BsmtFinSF2
## Similar to the variable above, this is the sqft for the
## second type of basement.  Example- if a home has a half
## finished and half unfinished basement, this will give the
## sqft of the second type of basement
typeof(df$BsmtFin.SF.2)
length(df$BsmtFin.SF.2)
table(df$BsmtFin.SF.2)
## We see the same issue here with the size
## Maybe the small sizes make sense if one room unfinished
## or a small area where water heater/furnace located unfinished?

hist(df$BsmtFin.SF.2)
## The vast majority of houses don't have two different
## types of basements in the same home

## Considering the size issues in BsmtFin.SF.1, are there any
## basements where basement 1 is smaller than basement2?
sum(df$BsmtFin.SF.2> df$BsmtFin.SF.1)
## we have 23 basements where the second type is larger than the 
## first.  lets explore total bsmt size for these
df3<-df$Total.Bsmt.SF[df$BsmtFin.SF.2> df$BsmtFin.SF.1]
table(df3)
## all of these sizes make sense, so maybe we don't have an issues
table(df$Total.Bsmt.SF)
## Looking at total, the smallest bsmt is 346 sqft, so it is
## more of a classification issue than a problem in the data

## Overall recommendation, consider interaction terms if we 
## want to consider effect of a basement

############################################################

## BSMT Unf SF
## Total unfinished sqft of basement
typeof(df$Bsmt.Unf.SF)
length(df$Bsmt.Unf.SF)
table(df$Bsmt.Unf.SF)
## only 28 homes with 0sqft of unfinished basement, some are
## very small, but could be for utility room
hist(df$Bsmt.Unf.SF)
## Expectedly we have a right skew to this variable, 
hist(log10(df$Bsmt.Unf.SF))
## Log gets us closer to a nml distribution if we want to use this variable

plot(df$Bsmt.Unf.SF, df$SalePrice, main = "SalePrice vs Bsmt.Unf.SF", col = "blue")
abline(lm(SalePrice~ Bsmt.Unf.SF, data= df), col ="red")
## Appears to be a slightly positive relationship

plot(log(df$Bsmt.Unf.SF), df$SalePrice, main = "SalePrice vs Bsmt.Unf.SF", col = "blue")
## seems like less of a relationship based on the plot

############################################################

## Total Bsmt SF
## Total sqft of basement
typeof(df$Total.Bsmt.SF)
length(df$Total.Bsmt.SF)
table(df$Total.Bsmt.SF)
## only 7 homes have no basement, sizes here make sense
hist(df$Total.Bsmt.SF)
## Have a slight R skew, imagine this distribution closely resembles
## total home sqft histogram
hist(log(df$Total.Bsmt.SF))
## log transformation makes this much more normal
summary(df$Total.Bsmt.SF)

plot(df$Total.Bsmt.SF, df$SalePrice, main = "SalePrice vs Total.Bsmt.SF", col = "blue")
abline(lm(SalePrice~ Total.Bsmt.SF, data = df), col = "red")
## Seems to be a good linear relationship

plot(log(df$Total.Bsmt.SF), df$SalePrice, main = "SalePrice vs Total.Bsmt.SF", col = "blue")

ModelTotBsmtSF <- lm(SalePrice ~ Total.Bsmt.SF, data =df)
coeftest(ModelTotBsmtSF, vcov= vcovHC)
## statistically significant by self
plot(ModelTotBsmtSF)
## some issues with the residuals v fitted at the extremes, but not a lot
## of values, looks good for majority of values
## QQ plot looks good, a little skew at the high end
## Scale location also good in middle, but heteroskedastic at extremes
## A couple values close to .5 Cook's distance, but none reach it

## Seems that bsmt sqft should be included.  Possbily basement type if interaction terms
## give good information

############################################################

## 1st Flr SF
## Note that R puts an X infront of numbers that start column names in DF
## self explanatory variable
typeof(df$X1st.Flr.SF)
length(df$X1st.Flr.SF)
table(df$X1st.Flr.SF)
## smallest home has 372 sqft on first floor, this is small, but could make sense
hist(df$X1st.Flr.SF)
## appears to be close to normally distributed, slight skew R
summary(df$X1st.Flr.SF)

plot(df$X1st.Flr.SF, df$SalePrice, main="SalePrice vs 1st.Flr.SF", col="blue")
abline(lm(SalePrice ~ X1st.Flr.SF, data= df), col="red")
## Appears to be a strong linear relationship, which makes sense intuitively

Model1stFlrSqft <- lm(SalePrice ~ X1st.Flr.SF, data= df)
coeftest(Model1stFlrSqft, vcov = vcovHC)
## T 9.59 and P > 2e-16
plot(Model1stFlrSqft)
## some heteroskedasticity in residuals v fitted
## QQ plot good, some variance at extremes
## How do we interpret the scale location?
## Residual v leverage okay, #45 still close to Cook's distance
## I'd imagine that this somewhat matters, but this is will closely correlated
## with total sqft

############################################################

## 2nd Flr SF
typeof(df$X2nd.Flr.SF)
length(df$X2nd.Flr.SF)
table(df$X2nd.Flr.SF)
## 195 homes without a second floor.  Also, many have small second floor
hist(df$X2nd.Flr.SF)
## appears to be a somewhat normal distribution, if you remove the 0s
hist(df$X2nd.Flr.SF[df$X2nd.Flr.SF>0])
summary(df$X2nd.Flr.SF)
## mean 320, but median 0
summary(df$X2nd.Flr.SF[df$X2nd.Flr.SF>0])
## Mean and median close for homes with a second floor, which is consistent
## with thoughts of being close to normal
plot(df$X2nd.Flr.SF, df$SalePrice, main="SalePrice vs 2nd.Flr.SF", col="blue")
abline(lm(SalePrice~X2nd.Flr.SF, data=df), col = "red")

plot(df$X2nd.Flr.SF[df$X2nd.Flr.SF>0], df$SalePrice[df$X2nd.Flr.SF>0], main="SalePrice vs 2nd.Flr.SF", col="blue")
abline(lm(SalePrice[df$X2nd.Flr.SF>0]~X2nd.Flr.SF[df$X2nd.Flr.SF>0], data=df), col = "red")
## By removing the 0s, we see a somewhat linear relationship

Model2ndFlrSqft <- lm(SalePrice~X2nd.Flr.SF, data=df)
coeftest(Model2ndFlrSqft, vcov= vcovHC)
## statistically significant at .01
plot(Model2ndFlrSqft)
## some issues in residuals v fitted
## qq good between -1 and 1, then skews
## scale location also issue w heteroskedasticity

############################################################

## Low Qual Fin SF
## Total of low quality finish for all floors
typeof(df$Low.Qual.Fin.SF)
length(df$Low.Qual.Fin.SF)
table(df$Low.Qual.Fin.SF)
hist(df$Low.Qual.Fin.SF)
## There are only two non-zero values.  No need to go further here.



############################################################

## Pool Area
## Pool area in sqft, unclear if that means area around pool or just pool itself
## Don't think that really matters for our purposes
typeof(df$Pool.Area)
length(df$Pool.Area)
table(df$Pool.Area)
hist(df$Pool.Area)
## None of these homes have pool areas, nothing to do here.


############################################################

## House Style
## 1Story	One story
## 1.5Fin	One and one-half story: 2nd level finished
## 1.5Unf	One and one-half story: 2nd level unfinished
## 2Story	Two story
## 2.5Fin	Two and one-half story: 2nd level finished
## 2.5Unf	Two and one-half story: 2nd level unfinished
## SFoyer	Split Foyer
## SLvl	Split Level
typeof(df$House.Style)
## weird that integer
length(df$House.Style)
table(df$House.Style)
## house types
table(df$House.Style)

plot(df$House.Style, df$SalePrice, main = "SalePrice v House Style")
levels(df$House.Style)
x1<- mean(df$SalePrice[df$House.Style=="1.5Fin"])
y1<-sd(df$SalePrice[df$House.Style=="1.5Fin"])

x2<-mean(df$SalePrice[df$House.Style=="1Story"])
y2<-sd(df$SalePrice[df$House.Style=="1Story"])

x3<-mean(df$SalePrice[df$House.Style=="2.5Unf"])
y3<-sd(df$SalePrice[df$House.Style=="2.5Unf"])

x4<-mean(df$SalePrice[df$House.Style=="2Story"])
y4<-sd(df$SalePrice[df$House.Style=="2Story"])

x5<-mean(df$SalePrice[df$House.Style=="SFoyer"])
y5<-sd(df$SalePrice[df$House.Style=="SFoyer"])

x6<-mean(df$SalePrice[df$House.Style=="SLvl"])
y6<-sd(df$SalePrice[df$House.Style=="SLvl"])

xMeans <-c(x1, x2, x3, x4, x5, x6)
yStdDev <- c(y1, y2, y3, y4, y5, y6)
housetypes <- c("1.5Fin", "1Story", "2.5Unf", "2Story", "SFoyer", "SLvl")
cbind(housetypes, xMeans,yStdDev)

############################################################

## Roof Style
## Flat	Flat
## Gable	Gable
## Gambrel	Gabrel (Barn)
## Hip	Hip
## Mansard	Mansard
## Shed	Shed
typeof(df$Roof.Style)
length(df$Roof.Style)
table(df$Roof.Style)

plot(df$Roof.Style, df$SalePrice, main= "SalePrice v Roof.Style")
## note vast majority are Gable roofs, probably have issues with low 
## number of shed, Mansard, Gambrel, and Flat roofs

levels(df$Roof.Style)
xx1<- mean(df$SalePrice[df$Roof.Style=="Flat"])
yy1<-sd(df$SalePrice[df$Roof.Style=="Flat"])

xx2<-mean(df$SalePrice[df$Roof.Style=="Gable"])
yy2<-sd(df$SalePrice[df$Roof.Style=="Gable"])

xx3<-mean(df$SalePrice[df$Roof.Style=="Gambrel"])
yy3<-sd(df$SalePrice[df$Roof.Style=="Gambrel"])

xx4<-mean(df$SalePrice[df$Roof.Style=="Hip"])
yy4<-sd(df$SalePrice[df$Roof.Style=="Hip"])

xx5<-mean(df$SalePrice[df$Roof.Style=="Mansard"])
yy5<-sd(df$SalePrice[df$Roof.Style=="Mansard"])

xx6<-mean(df$SalePrice[df$Roof.Style=="Shed"])
yy6<-sd(df$SalePrice[df$Roof.Style=="Shed"])

xxMeans <-c(xx1, xx2, xx3, xx4, xx5, xx6)
yyStdDev <- c(yy1, yy2, yy3, yy4, yy5, yy6)
housetypes <- c("Flat", "Gable", "Gambrel", "Hip", "Mansard", "Shed")
cbind(housetypes, xxMeans,yyStdDev)

## Mean is about the same, some high values with Hip roof, but
## probably nothing statistically significant just on roof, low number
## of observations for most values.



############################################################

## Roof Matl
#ClyTile	Clay or Tile
#CompShg	Standard (Composite) Shingle
#Membran	Membrane
#Metal	Metal
#Roll	Roll
#Tar&Grv	Gravel & Tar
#WdShake	Wood Shakes
#WdShngl	Wood Shingles
typeof(df$Roof.Matl)
length(df$Roof.Matl)
table(df$Roof.Matl)
## They are almost all CompShg, no need to go further since no variation

############################################################

## Paved Drive
## Y	Paved 
## P	Partial Pavement
## N	Dirt/Gravel
typeof(df$Paved.Drive)
length(df$Paved.Drive)
table(df$Paved.Drive)

plot(df$Paved.Drive, df$SalePrice, main = "SalePrice vs Paved.Drive")
## Not too surprising, the mean increases with a paved drive.

ModelPavedDrive<- lm(SalePrice~ Paved.Drive, data= df)
plot(ModelPavedDrive)
## Not really sure how to interpret these, suggestions?

############################################################

## Pool QC
typeof()
length()
table(df$Pool.QC)
sum(is.na(df$Pool.QC))
## no data


############################################################

## Fence
## GdPrv	Good Privacy
## MnPrv	Minimum Privacy
## GdWo	Good Wood
## MnWw	Minimum Wood/Wire
## NA	No Fence
typeof(df$Fence)
length(df$Fence)
table(df$Fence)
levels(df$Fence)
sum(is.na(df$Fence))

plot(df$Fence, df$SalePrice, "SalePrice vs Fence Type", col="Blue")
##I don't think we have enough data to make this meaningful













