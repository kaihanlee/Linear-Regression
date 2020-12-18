#import library
library(MASS)
library(boot)

###Q1###
oecd = read.table("OECD_Pensions.txt", header=T)  #read data

benefits = oecd$Benefits  #declare benefits
assets = oecd$Assets      #declare assets

oecd.model = lm(benefits~assets)  #simple linear model
summary(oecd.model)   #summary of simple linear model
par(mfrow=c(2,2))     #fit 4 plots into 1 interface
plot(oecd.model,which=1:4)    #residual plots

par(mfrow=c(1,1))     #fit 1 plot into 1 interface
#plot graph of benefits against assets
plot(benefits~assets,main="Plot of Benefits against Assets",xlab="Assets",ylab="Benefits")
abline(oecd.model,col="red",lty=2)    #plot best fit line

oecd.model1 = lm(benefits[-36]~assets[-36])  #simple linear model (revised)
summary(oecd.model1)   #summary of simple linear model (revised)
#plot graph of benefits against assets (revised)
plot(benefits[-36]~assets[-36],main="Plot of Benefits against Assets (revised)",xlab="Assets",ylab="Benefits")
abline(oecd.model1,col="red",lty=2)    #plot best fit line

logoecd.model = lm(log(benefits)~log(assets))   #simple linear model for log
summary(logoecd.model)    #summary of simple linear model for log
par(mfrow=c(2,2))     #fit 4 plots into 1 interface
plot(logoecd.model,which=1:4)   #residual plots

par(mfrow=c(1,1))       #fit 1 plot into 1 interface
#plot graph of log(benefits) against log(assets)
plot(log(benefits)~log(assets),main="Plot of log of Benefits against log of Assets",xlab="ln(Assets)",ylab="ln(Benefits)")
abline(logoecd.model,col="red",lty=2)    #plot best fit line

#Chi-squared goodness of fit test using equal area cells###
Chi.sq <- function(Z, N){
  mu <- mean(Z)     #degree of freedom
  n <- length(Z)    #number of data
  sigma <- sd(Z)    #standard deviation of data
  exp <- n/N        #with equal-probability intervals
  breaks <- qnorm(seq(0,1,by=1/N),mu,sigma)
  cut <- cut(Z,breaks,right=F)
  table <- table(cut)
  obs = hist(Z, breaks = breaks, plot=F, right=F)$counts
  
  Xsq <- sum( (obs - exp)^2/exp)    #Chi-square test
  pval <- 1 - pchisq(Xsq, (N-2-1))    #calculate p-value
  stdarea <- c(Xsq,pval) #return values
  names(stdarea) <- c("Chis2","Sig.Pr")

  return(stdarea)
}
Chi.sq(log(assets),6)    #6 equal-probability cells
Chi.sq(log(assets),5)    #5 equal-probability cells

###Q2###
counties = read.table("USA_Counties.txt",header=T)  #read data  
attach(counties)

medinc = counties$Medinc  #declare medinc
unemp = counties$Unemp    #declare unemp
metro = counties$Metro    #declare metro

count.modb <- lm(log(medinc) ~ log(unemp)* factor(metro))   #multiple linear regression model for log
summary(count.modb)   #summary output for multiple linear regression model for log

count.modc <- lm(log(medinc) ~ log(unemp)+ factor(metro))   #multiple linear regression model for log without interaction term
summary(count.modc)   #summary output for multiple linear regression model for log without interaction term

count.mod0 = lm(log(medinc)[metro=="0"]~log(unemp)[metro=="0"]) #multiple linear regression for log (non-metropolitan)
count.mod1 = lm(log(medinc)[metro=="1"]~log(unemp)[metro=="1"]) #multiple linear regression for log (metropolitan)

#plot graph of log(medinc) against log(unemp)
plot(log(medinc)~log(unemp),type="n",main="Plot of log of Median Household Income against log of Unemployment Rate",
     xlab="log(Unemployment Rate)",ylab="log(Median Household Income)")
points(log(medinc)[metro=="0"]~log(unemp)[metro=="0"],pch=0)    #plot points for non-metropolitan
points(log(Medinc)[Metro=="1"]~log(Unemp)[Metro=="1"],pch=1,col="red")  #plot points for metropolitan
abline(count.mod0)    #best fit line for non-metropolitan
abline(count.mod1,col="red")  #best fit line for metropolitan
legend(2.5,11.8,legend=c("Non-Metropolitan","Metropolitan"),pch=c(0,1),col=c("black","red"),cex=0.8)

par(mfrow=c(2,2))   #fit 4 plots into 1 interface
plot(count.modb,which=1:4)   #residual plots

anova(count.modb)   #analysis of variance for multiple linear regression model for log

###Q3###
volcanoes = read.table("volcanoes.txt",header=T)  #read data

name = volcanoes$Name             #declare name
elevation = volcanoes$Elevation   #declare elevation
type = volcanoes$Type             #declare type
eruptions = volcanoes$Eruptions   #declare eruptions

#generalised linear model for both expanatory variables
model1 = glm(eruptions~elevation+factor(type), family = poisson); summary(model1)
1-pchisq(model1$deviance,model1$df.residual)    #p-value for chi-squared test
#generalised linear model for type as expanatory variable
model2 = glm(eruptions~factor(type), family = poisson); summary(model2)
1-pchisq(model2$deviance-model1$deviance,model2$df.residual-model1$df.residual)    #p-value for chi-squared test
#generalised linear model for elevation as expanatory variable
model3 = glm(eruptions~elevation, family = poisson); summary(model3)
1-pchisq(model3$deviance-model1$deviance,model3$df.residual-model1$df.residual)    #p-value for chi-squared test

#plot graph of eruptions against elevation and type as explanatory variables
plot(eruptions ~ elevation, type="n", xlab="Height above Sea Level (m)", ylab="Number of Eruptions"
     , main = "Plot of No. of Eruptions against Height above Sea Level")
points(eruptions[type=="Stratovolcano"] ~ elevation[type=="Stratovolcano"], pch=0)  #plot points for Statovolcano
points(eruptions[type=="Caldera"] ~ elevation[type=="Caldera"], pch=1, col="red")   #plot points for Caldera
legend(0,4,legend=c("Stratovolcano","Caldera"),pch=c(0,1),col=c("black","red"),cex=0.8)

par(mfrow=c(2,2))   #fit 4 plots into 1 interface
plot(model1,which=1:4)  #residual plots

#residual plots
#plot graph of residuals against elevation
plot(residuals(model2)~elevation,type="n",xlab="Height above sea level (m)",ylab="Deviance residuals")
points(residuals(model2)[type=="Stratovolcano"] ~ elevation[type=="Stratovolcano"], pch=0)  #plot points for Statovolcano
points(residuals(model2)[type=="Caldera"] ~ elevation[type=="Caldera"], pch=1, col="red")   #plot points for Caldera
abline(h=0,col="red",lty=2)   #plot horizontal line at 0
legend(0,2,legend=c("Stratovolcano","Caldera"),pch=c(0,1),col=c("black","red"),cex=0.8)     #legend

#plot graph of residuals against fitted values
plot(residuals(model2)~fitted.values(model2),type="n",xlab="Fitted values",ylab="Deviance residuals")
points(residuals(model2)[type=="Stratovolcano"] ~ fitted.values(model2)[type=="Stratovolcano"], pch=0)  #plot points for Statovolcano
points(residuals(model2)[type=="Caldera"] ~ fitted.values(model2)[type=="Caldera"], pch=1, col="red")   #plot points for Caldera
abline(h=0,col="red",lty=2)   #plot horizontal line at 0
legend(0.60,2,legend=c("Stratovolcano","Caldera"),pch=c(0,1),col=c("black","red"),cex=0.8)  #legend

#plot graph of deviance vs pearson residuals againts elevation
plot(residuals(model2,type="pear")~elevation, pch=2,xlab="Height above sea level (m)",ylab="Residuals",
     main="Deviance vs Pearson residuals against Elevation")
points(residuals(model2)~elevation, pch=1,col="red")  #plot points for deviance residuals
abline(h=0,col="red",lty=2)   #plot horizontal line at 0
legend(0,2.8,legend=c("Pearson","Deviance"),col=c("black","red"),pch=c(2,1),cex=0.8)  #legend

#plot boxplot of eruptions against type as explanatory variable
plot(eruptions ~ factor(type), xlab="Type of Volcano", ylab="Number of Eruptions", 
     main="Boxplot of Number of Eruptions of Different Types of Volcanoes", horizontal=T)
