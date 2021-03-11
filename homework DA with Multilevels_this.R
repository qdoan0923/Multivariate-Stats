# "Discriminant Analysis"	
# Quint Doan, adapted from "JDRS"	


# ##  Discriminant Analysis in R 	
# ####  Multivariate Statistics	
# ####  J. Reuning-Scherer	


# First, some useful packages for performing DA.	

library(MASS)	
library(biotools)	
library(DiscriMiner)	
library(klaR)	
library(car)	
library(dplyr)	
library(lubridate)	
library(ggplot2)	
library(ggExtra)	
library(corrplot)
library(PerformanceAnalytics)

# ##  Endangered Species	


#get data	
WB <- read.csv("/Users/quintdoan/Documents/Yale/Spring 2021/Multivariate/Edited_Data.csv", header = T, as.is = F)	

#Read names of plot
names(WB)	


#Get subset of data that is complete for relevant variables	
WB2 <- WB[ ,c("Species_Name","X1.1_ActivityCycle",
              "X5.1_AdultBodyMass_g","X8.1_AdultForearmLen_mm",
              "X13.1_AdultHeadBodyLen_mm","X2.1_AgeatEyeOpening_d",
              "X3.1_AgeatFirstBirth_d","X18.1_BasalMetRate_mLO2hr",
              "X5.2_BasalMetRateMass_g","X6.1_DietBreadth",
              "X7.1_DispersalAge_d","X9.1_GestationLen_d",
              "X12.1_HabitatBreadth","X22.1_HomeRange_km2",
              "X22.2_HomeRange_Indiv_km2","X14.1_InterbirthInterval_d",
              "X15.1_LitterSize","X16.1_LittersPerYear",
              "X17.1_MaxLongevity_m","X5.3_NeonateBodyMass_g",
              "X13.2_NeonateHeadBodyLen_mm","X21.1_PopulationDensity_n.km2",
              "X10.1_PopulationGrpSize","X23.1_SexualMaturityAge_d",
              "X10.2_SocialGrpSize","X24.1_TeatNumber",      
              "X25.1_WeaningAge_d","X5.4_WeaningBodyMass_g",    
              "X13.3_WeaningHeadBodyLen_mm", "X5.5_AdultBodyMass_g_EXT",
              "X16.2_LittersPerYear_EXT", "X5.6_NeonateBodyMass_g_EXT",
              "X5.7_WeaningBodyMass_g_EXT", "X26.1_GR_Area_km2",
              "X26.2_GR_MaxLat_dd", "X26.3_GR_MinLat_dd",
              "X26.4_GR_MidRangeLat_dd", "X26.5_GR_MaxLong_dd",
              "X26.6_GR_MinLong_dd","X26.7_GR_MidRangeLong_dd",
              "X27.1_HuPopDen_Min_n.km2", "X27.2_HuPopDen_Mean_n.km2",
              "X27.3_HuPopDen_5p_n.km2", "X27.4_HuPopDen_Change",
              "X28.1_Precip_Mean_mm", "X28.2_Temp_Mean_01degC",
              "X30.1_AET_Mean_mm", "X30.2_PET_Mean_mm",
              "USA_Endangered", "Extinct", 
              "IUCN_LISTED")]

#We need complete cases for code to work	
WB2<- WB2[complete.cases(WB2), ]	
dim(WB2)


# For starters, make correlation matrices and use visual representations.	
#Get subset of data that is complete for relevant variables	
WB3 <- WB[ ,c("Species_Name","X1.1_ActivityCycle",
              "X5.1_AdultBodyMass_g","X6.1_DietBreadth",
              "X12.1_HabitatBreadth","X22.1_HomeRange_km2",
              "X15.1_LitterSize","X17.1_MaxLongevity_m","X21.1_PopulationDensity_n.km2",
              "X26.4_GR_MidRangeLat_dd", "X27.2_HuPopDen_Mean_n.km2",
              "X28.1_Precip_Mean_mm", "X28.2_Temp_Mean_01degC",
              "USA_Endangered", "Extinct", 
              "IUCN_LISTED")]

#Create a column that shows if something is extinct or endangered
WB3$Extinct=2*(WB3$Extinct)
#Create a column that shows if something is extinct or endangered
#There is not enough of these points to define a 12 dimensional space
WB3$Extinct_Endangered=(WB3$Extinct+WB3$IUCN_LISTED)

#delete datapoints that are both extinct and listed as endangered
WB3 <- WB3[WB3$Extinct_Endangered != 3,]

#make correlation matrix to see if DA will work well - remove column of species names	
round(cor(WB3[, 2:13]), 2)	

#Cooler visual representation of correlations	
corrplot(cor(WB3[, 2:13]), method = "ellipse")	

#Cooler visual representation of correlations	
#Order option orders variables based on data order, alphabetically, results of cluster analysis, etc.	
#  See help file or link above to get details.	

corrplot(cor(WB3[,2:13]),method = "ellipse", order="FPC")	
corrplot(cor(WB3[,2:13]),method = "ellipse", order="AOE")	
corrplot(cor(WB3[,2:13]),method = "ellipse", order="hclust")	

#Lots of options here - but my personal favorite	
corrplot.mixed(cor(WB3[,2:13]), lower.col = "black", upper = "ellipse", tl.col = "black", number.cex = .7, order = "hclust", tl.pos = "lt", tl.cex = .7)	


# It's also important to check for linearity.	

#make matrix plot to check for linearity	
plot(WB3[, 2:13], pch = 19, cex = .7, col = 'red', main = "Matrix plot of WB raw data")	

#Here is a cool way to look for non-linearity, get correlation, make histograms all at once.	
chart.Correlation(WB3[, 2:13], histogram = TRUE, pch = 19)	


#Make FUNCTION categorical	
WB3$Endangered_or_Extinct <- as.factor(WB3$Extinct_Endangered)	
levels(WB3$Endangered_or_Extinct) <- c("Healthy", "Endangered", "Extinct")	


# Check for multivariate normality in each group (Fealthy, Endangered, Extinct)	


#get online function	
source("http://www.reuningscherer.net/multivariate/R/CSQPlot.r.txt")	

#examine multivariate normality within each group	
CSQPlot(WB3[WB3$Endangered_or_Extinct == "Healthy", 2:13], label = "Healthy")	
CSQPlot(WB3[WB3$Endangered_or_Extinct == "Endangered", 2:13], label = "Endangered")	
CSQPlot(WB3[WB3$Endangered_or_Extinct == "Extinct", 2:13], label = "Extinct")	


#make matrix plot to look at differences between groups	
plot(WB3[,2:13], col = WB3[,2]+2, pch = WB3[,2]+15, cex = 1.2)	

# Seems like covariance 'footprints' may not be the same between groups	

names(WB3)	

#visually compare sample covariance matrices	
print("Covariance Matrix for Extinct")	
cov(WB3[WB3$Endangered_or_Extinct=="Extinct", 2:13])	
print("Covariance Matrix for Endangered")	
cov(WB3[WB3$Endangered_or_Extinct=="Endangered", 2:13])	
print("Covariance Matrix for Healthy")	
cov(WB3[WB3$Endangered_or_Extinct=="Healthy", 2:13])	


#compare standard deviations in each group	
sumstats <- round(sqrt(aggregate(WB3[,2:13], by = list(WB3[,17]),FUN=var)),3)[,-1]	
rownames(sumstats) <- c("Healthy","Endangered","Extinct")	
print("Standard Deviations by Group")	
sumstats	

#calculate Box's M statistic	
boxM(WB3[,2:13], WB4$Endangered_or_Extinct)


#It does not, lets assess where the non-normality lies

#Get a normal quantile plot of for the eight variables, raw scale
qqPlot(WB3$X1.1_ActivityCycle, col = 'red', pch = 19)	
qqPlot(WB3$X5.1_AdultBodyMass_g, col = 'red', pch = 19)	
qqPlot(WB3$X6.1_DietBreadth, col = 'red', pch = 19)	
qqPlot(WB3$X12.1_HabitatBreadth, col = 'red', pch = 19)	
qqPlot(WB3$X22.1_HomeRange_km2, col = 'red', pch = 19)	
qqPlot(WB3$X15.1_LitterSize, col = 'red', pch = 19)	
qqPlot(WB3$X17.1_MaxLongevity_m, col = 'red', pch = 19)
qqPlot(WB3$X21.1_PopulationDensity_n.km2, col = 'red', pch = 19)
qqPlot(WB3$X26.4_GR_MidRangeLat_dd, col = 'red', pch = 19)
qqPlot(WB3$X27.2_HuPopDen_Mean_n.km2, col = 'red', pch = 19)
qqPlot(WB3$X28.1_Precip_Mean_mm, col = 'red', pch = 19)
qqPlot(WB3$X28.2_Temp_Mean_01degC, col = 'red', pch = 19)

#perform the relevant transformation
WB3$LogActivityCycle=log(WB3$X1.1_ActivityCycle)
#Display normal quantile plot
qqPlot(WB3$LogActivityCycle, col = 'red', pch = 19)

#perform the relevant transformation
WB3$LogAdultBodyMass_g=log(WB3$X5.1_AdultBodyMass_g)
#Display normal quantile plot
qqPlot(WB3$LogAdultBodyMass_g, col = 'red', pch = 19)

#perform the relevant transformation
WB3$LogDietBreadth=log(WB3$X6.1_DietBreadth)
#Display normal quantile plot
qqPlot(WB3$LogDietBreadth, col = 'red', pch = 19)	

#perform the relevant transformation
WB3$LogHabitatBreadth=log(WB3$X12.1_HabitatBreadth)
#Display normal quantile plot
qqPlot(WB3$LogHabitatBreadth, col = 'red', pch = 19)

#perform the relevant transformation
WB3$LogHomeRange_km2=log(WB3$X22.1_HomeRange_km2)
#Display normal quantile plot
qqPlot(WB3$LogHomeRange_km2, col = 'red', pch = 19)

#perform the relevant transformation
WB3$LogLitterSize=log(WB3$X15.1_LitterSize)
#Display normal quantile plot
qqPlot(WB3$LogLitterSize, col = 'red', pch = 19)

#perform the relevant transformation
WB3$LogMaxLongevity_m=log(WB3$X17.1_MaxLongevity_m)
#Display normal quantile plot
qqPlot(WB3$LogMaxLongevity_m, col = 'red', pch = 19)

#perform the relevant transformation
WB3$LogPopulationDensity_n.km2=log(WB3$X21.1_PopulationDensity_n.km2)
#Display normal quantile plot
qqPlot(WB3$LogPopulationDensity_n.km2, col = 'red', pch = 19)

#perform the relevant transformation (add 90 so all values are positive, it is lattitude)
WB3$LogGR_MidRangeLat_dd=log(WB3$X26.4_GR_MidRangeLat_dd+90)
#Display normal quantile plot
qqPlot(WB3$LogGR_MidRangeLat_dd, col = 'red', pch = 19)

#perform the relevant transformation (add one because some values are zero)
WB3$LogHuPopDen_Mean_n.km2=log(WB3$X27.2_HuPopDen_Mean_n.km2+1)
#Display normal quantile plot
qqPlot(WB3$LogHuPopDen_Mean_n.km2, col = 'red', pch = 19)

#perform the relevant transformation
WB3$LogPrecip_Mean_mm=log(WB3$X28.1_Precip_Mean_mm)
#Display normal quantile plot
qqPlot(WB3$LogPrecip_Mean_mm, col = 'red', pch = 19)

#perform the relevant transformation (add 300 to make values positive)
WB3$LogTemp_Mean_01degC=log(WB3$X28.2_Temp_Mean_01degC+300)
#Display normal quantile plot
qqPlot(WB3$LogTemp_Mean_01degC, col = 'red', pch = 19)

#Add the columns to a new matrix of transformed data
WB4 <- WB3[ ,c("Species_Name","LogActivityCycle",
               "LogAdultBodyMass_g","LogDietBreadth",
               "LogHabitatBreadth","LogHomeRange_km2",
               "LogLitterSize","LogMaxLongevity_m",
               "LogPopulationDensity_n.km2","LogGR_MidRangeLat_dd",
               "LogHuPopDen_Mean_n.km2", "LogPrecip_Mean_mm",
               "LogTemp_Mean_01degC", "USA_Endangered", 
               "Extinct", "IUCN_LISTED", "Extinct_Endangered",
               "Endangered_or_Extinct")]

#examine multivariate normality within each group	
CSQPlot(WB4[WB4$Endangered_or_Extinct == "Healthy", 2:13], label = "Healthy")
CSQPlot(WB4[WB4$Endangered_or_Extinct == "Endangered", 2:13], label = "Endangered")
CSQPlot(WB4[WB4$Endangered_or_Extinct == "Extinct", 2:13], label = "Extinct")


#make matrix plot to look at differences between groups	
plot(WB4[,2:13], col = WB4[,17]+2, pch = WB4[,17]+15, cex = 1.2)	

#visually compare sample covariance matrices	
print("Covariance Matrix for Not Endangered")	
cov(WB4[WB4$Endangered_or_Extinct == "Healthy", 2:13])	
print("Covariance Matrix for Endangered")	
cov(WB4[WB4$Endangered_or_Extinct == "Endangered", 2:13])	
print("Covariance Matrix for Extinct")	
cov(WB4[WB4$Endangered_or_Extinct == "Extinct", 2:13])	

#compare standard deviations in each group	
sumstats <- round(sqrt(aggregate(WB4[,2:13], by = list(WB4[,17]),FUN=var)),3)[,-1]	
rownames(sumstats) <- c("Healthy","Endangered","Extinct")	
print("Standard Deviations by Group")	
sumstats	

#calculate Box's M statistic	
boxM(WB4[,2:13], WB4$Endangered_or_Extinct)	


# It appears that the covariances matrices are NOT the same between groups - so we fit both linear DA and quadratic DA	

# ####Linear DA	


#run linear discriminant analysis	
WB4.disc <- lda(WB4[,2:13], grouping = WB4$Endangered_or_Extinct)	
summary(WB4.disc)	

#get univarite and multivariate comparisons	
WB4.manova <- manova(as.matrix(WB4[,2:13]) ~ WB4$Endangered_or_Extinct)	
summary.manova(WB4.manova, test = "Wilks")	
summary.aov(WB4.manova)	


# Classification Results - note have to run lda separately with CV=TRUE option if you want both raw and cross-validated results	


# raw results	


# total percent correct	
round(sum(diag(prop.table(ctraw))), 2)	

#cross validated results	
WB4.discCV <- lda(WB4[,2:13], grouping = WB4[,17], CV = TRUE)	
(ctCV <- table(WB4$Endangered_or_Extinct, WB4.discCV$class))	

# total percent correct	
round(sum(diag(prop.table(ctCV))),2)	

# *NOTE - might make score plot here, but only one discriminant function, so we're in one dimension - i.e. make boxplot or other uni-dimensional plot make boxplot in direction of linear discriminant function.	


#get the scores - matrix product of original variables with DA coefficients	
scores <- as.matrix(WB4[,2:13])%*%matrix(c(WB4.disc$scaling), ncol = 2)	

boxplot(scores ~ WB4$Endangered_or_Extinct, lwd = 3, col = c("red","blue","green"), horizontal = T, main = "Endangered Species Discriminant Scores by Function", ylab = "Function")	


# ####Quadratic DA (i.e. assume unequal covariance matrices)	


#run quadratic discriminant analysis	
WB4Q.disc <- qda(WB4[,2:13], grouping = WB4$Endangered_or_Extinct)	
summary(WB4Q.disc)	

# raw results - more accurate than using linear DA	
ctrawQ <- table(WB4$Endangered_or_Extinct, predict(WB4Q.disc)$class)	
ctrawQ	

#cross validated results - better than CV for linear DA	
WB4.discCVQ <- qda(WB4[,2:13], grouping = WB4[,17], CV = TRUE, )	
ctCVQ <- table(WB4$Endangered_or_Extinct, WB4.discCVQ$class)	
ctCVQ	

#cross validated results - better than CV for linear DA	(the Priors don't help)
WB4.discCVQ1 <- qda(WB4[,2:13], grouping = WB4[,17], prior = c(.33, .33, .34), CV = TRUE, )	
ctCVQ2 <- table(WB4$Endangered_or_Extinct, WB4.discCVQ1$class)	
ctCVQ2



# Another way to do quadratic DA with cross-validtion is to use the quaDA() function in the DiscriMiner package.	


# quadratic discriminant analysis with cross-validation 	
# DiscriMiner package	

my_qua2 <-  quaDA(WB4[,2:13], WB4$Endangered_or_Extinct, validation = "crossval") 	
names(my_qua2)	
my_qua2$confusion 	
my_qua2$error_rate	

#Do quadratic linear analysis without crossvalidation
my_qua2 <-  quaDA(WB4[,2:13], WB4$Endangered_or_Extinct) 	
names(my_qua2)	
my_qua2$confusion 	
my_qua2$error_rate	



# ##Stepwise DA (both linear and quadratic)	

# It may be that we don't need all the variables we're considering.   Try Stepwise DA both linear and quadratic.	

# Note that the stepclass() function uses what is called 'n-fold' cross-validation.   It leaves a random fraction of the data out when calculating the best variables to use.  As a result, you can get DIFFERENT variables chosen each time.   The best way to get consistent results is to use fold=nrow(data).   You can also set a random seed, but this simply means you get the same random choice of variables each time! 	


#STEPWISE DA	
#Just FYI, run line below several times
(step1 <- stepclass(Endangered_or_Extinct ~ LogActivityCycle + LogAdultBodyMass_g +
                      LogDietBreadth + LogHabitatBreadth +
                      LogHomeRange_km2 + LogLitterSize +
                      LogMaxLongevity_m + LogPopulationDensity_n.km2 +
                      LogGR_MidRangeLat_dd + LogHuPopDen_Mean_n.km2 + 
                      LogPrecip_Mean_mm + LogTemp_Mean_01degC, data = WB4, method = "lda", direction = "both"))	

#Here is leave-one out classification which is relatively stable - keeps only LogActivityCycle
(step1 <- stepclass(Endangered_or_Extinct ~ LogActivityCycle + LogAdultBodyMass_g +
                      LogDietBreadth + LogHabitatBreadth +
                      LogHomeRange_km2 + LogLitterSize +
                      LogMaxLongevity_m + LogPopulationDensity_n.km2 +
                      LogGR_MidRangeLat_dd + LogHuPopDen_Mean_n.km2 + 
                      LogPrecip_Mean_mm + LogTemp_Mean_01degC, data = WB4, method = "lda", direction = "both", fold = nrow(WB4)))	
names(step1)	
step1$result.pm	

#Do stepwise quadratic DA - keeps only LogActivityCycle
(step3 <- stepclass(Endangered_or_Extinct ~ LogActivityCycle + LogAdultBodyMass_g +
                      LogDietBreadth + LogHabitatBreadth +
                      LogHomeRange_km2 + LogLitterSize +
                      LogMaxLongevity_m + LogPopulationDensity_n.km2 +
                      LogGR_MidRangeLat_dd + LogHuPopDen_Mean_n.km2 + 
                      LogPrecip_Mean_mm + LogTemp_Mean_01degC, data = WB4, method = "qda", direction = 'both', fold = nrow(WB4)))	

#plot results in space spanned by choosen variables	
#First, linear DA with N and GS - linear partition of space	
partimat(Endangered_or_Extinct ~ LogActivityCycle + LogAdultBodyMass_g, data = WB4, method = "lda")	

#Second, QDA - quadratic partition of space	
partimat(Endangered_or_Extinct ~ LogDietBreadth + LogHabitatBreadth, data = WB4, method = "qda")	

#Note that if all more than two variables, it does all pairs of variables - oddly, log population densit and longevity do alright!	
partimat(Endangered_or_Extinct ~ LogMaxLongevity_m + LogPopulationDensity_n.km2 , data = WB4, method = "qda")	

###########


#NEW	
#Test of significance of resulting discriminant functions	
source("http://www.reuningscherer.net/multivariate/R/discrim.r.txt")	


#Two inputs requires - dataframe with discriminating variables and vector with group membership	
discriminant.significance(WB4[, c("LogActivityCycle",
                                  "LogAdultBodyMass_g")], WB4$Endangered_or_Extinct)

#See how much is explained using standardized coefficiencts.
WB4lda1 <- lda(scale(WB4[, c("LogActivityCycle",
                            "LogAdultBodyMass_g")]), grouping = WB4$Endangered_or_Extinct)	
names(WB4lda1)	
summary(WB4lda1)	
WB4lda1



# More useful are the 'space' plots we've already seen	


#PLOT Predicted Regions based on two log(shape) variables 	
#LDA	
partimat(Endangered_or_Extinct~ LogActivityCycle + LogAdultBodyMass_g, data = WB4, method = "lda")	

#SCORE PLOTS for linear DA	
WB4lda <- lda(scale(WB4[, c("LogActivityCycle",
                              "LogAdultBodyMass_g","LogDietBreadth",
                              "LogHabitatBreadth","LogHomeRange_km2",
                              "LogLitterSize","LogMaxLongevity_m",
                              "LogPopulationDensity_n.km2","LogGR_MidRangeLat_dd",
                              "LogHuPopDen_Mean_n.km2")]), grouping = WB4$Endangered_or_Extinct)	
names(WB4lda)	
summary(WB4lda)	

#Calculate scores	
scores <- as.matrix(scale(WB4[, c("LogActivityCycle",
                                  "LogAdultBodyMass_g","LogDietBreadth",
                                  "LogHabitatBreadth","LogHomeRange_km2",
                                  "LogLitterSize","LogMaxLongevity_m",
                                  "LogPopulationDensity_n.km2","LogGR_MidRangeLat_dd",
                                  "LogHuPopDen_Mean_n.km2")]))%*%matrix(WB4lda$scaling, ncol = 2)	

wb4names <- names(summary(WB4[, 18]))	

#NOTE - if use cross-validation option, scores are calculated automatically	
plot(scores[,1], scores[,2], type = "n", main = "Linear DCA scores for Endangered data",	
     xlab = "DCA Axis 1", ylab = "DCA Axis 2")	

irisnames <- names(summary(WB4[, 5]))	

for (i in 1:3){	
  points(scores[WB4$Endangered_or_Extinct == wb4names[i], 1],	
         scores[WB4$Endangered_or_Extinct == wb4names[i], 2], col = i+1, pch = 15+i, cex = 1.1)	
}	
legend("topright", legend = wb4names, col = c(2:4), pch = c(15, 16, 17))	

require(graphics)


# add legend via mouse click
colfill<-c(2:(2+length(levels(cyl.f))))
legend(locator(1), levels(cyl.f), fill=colfill)

#make a kernel density function
install.packages("sm")
library(sm)
sm.density.compare(WB4$LogHuPopDen_Mean_n.km2, WB4$Extinct_Endangered, xlab="LogHuPopDen_Mean_n_KM2")
title(main="Kernel Density Function of Human Population Density")

scale_WB4 <- WB4
scale_WB4[,2:13] <- scale(WB4[,2:13])
scale_rs.disc <- lda(scale_WB4[,2:13], grouping = WB4$Endangered_or_Extinct)
scale_rs2.disc <- lda(scale_WB4[,2:3], grouping = WB4$Endangered_or_Extinct)
scale_rsq.disc <- qda(scale_WB4[,2:13], grouping = WB4$Endangered_or_Extinct)

scale_rsq.disc

scale_rs.disc
scale_rs2.disc


