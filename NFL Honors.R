library(corrplot)
library(plm)
library(dplyr)
library(lmtest)
library(ggplot2)
library(stargazer)
library(MASS)
library(tidyverse)
library(caret)
library(leaps)
library(sandwich)
library(skedastic)
library(car)
library(carData)
library(lmtest)
library(plm)
library(AER)
summary(Updated_NFL_Honors_Dataset_4_5)

sd(Updated_NFL_Honors_Dataset_4_5$AvgMV)

dataset <- Updated_NFL_Honors_Dataset_4_5[-c(2,4, 6,  8, 10, 12, 14, 19, 20, 21, 22, 24)]
cor(dataset)

RTP2009 <- Updated_NFL_Honors_Dataset_4_5$DP1
RTP2018 <- Updated_NFL_Honors_Dataset_4_5$DP2
PAT2015<-ifelse(Updated_NFL_Honors_Dataset_4_5$DPAT=="Yes",1,0)

str(Updated_NFL_Honors_Dataset_4_5)
cor(Updated_NFL_Honors_Dataset_4_5)
boxplot(Updated_NFL_Honors_Dataset_4_5$RealRev)

Winpercentlagged<-lag(Updated_NFL_Honors_Dataset_4_5$Winpercent, 1)
Playofflag <- lag(Updated_NFL_Honors_Dataset_4_5$Playoffs)                 
revlog<-log(Updated_NFL_Honors_Dataset_4_5$RealRev)

dpatfixed <- lm(APPG~PAT2015+Avgpassyds+Redzone+Thirddownconversion+NetTurnovers +factor(Code), data = Updated_NFL_Honors_Dataset_4_5)
summary(dpatfixed)

white(dpatfixed)

dwtest(dpatfixed) #inconclusive results, 1.57 lower bound and 1.78 upper bound (1.76 result)
bgtest(dpatfixed)

coeftest(dpatfixed, vcov. = vcovHC(dpatfixed))
resettest(dpatfixed)
vif(dpatfixed)


appgfixed <- lm(APPG~DP1+DP2+Avgpassyds+Redzone+Thirddownconversion+NetTurnovers+factor(Code), data = Updated_NFL_Honors_Dataset_4_5)
summary(appgfixed) #fixed effects better because higher r-squared
#linear model that is accounting for the individual components of each team, which is what a fixed model is
#how to explain negative sign in DP2. Should I switch DP3 with DP1?

coeftest(appgfixed, vcov. = vcovHC(appgfixed))

white(appgfixed)
dwtest(appgfixed) #(lower bound : 1.55; upper bound ; 1.80)
resettest(appgfixed) #quality factors, mental health factors, other omitted vairables involved with an average points per game function, omitted variable bias

phtest(appgfixed, appgrandom) #cannot run a hausman test on a panel-linear regression model a regular ordinary least squares regression



stargazer(appgfixed, digits = 4, report = "cv*s", type = "text", out = "APPG RTP Model 2.html")
stargazer(dpatfixed, digits = 4, report = "cv*s", type = "text", out = "APPG PAT Model 2.html")
stargazer(logrevfixed, digits = 4, report = "cv*s", type = "text", out = "Revenue Model 2.html")
teamrevfixed <- lm(RealRev~APPG+Winpercentlagged+Probowl+AvgMV+factor(Code)+Playoffs, data = Updated_NFL_Honors_Dataset_4_5)
summary(teamrevfixed)

logrevfixed <- lm(revlog~APPG+Winpercentlagged+Probowl+AvgMV+Playofflag+factor(Code), data = Updated_NFL_Honors_Dataset_4_5)
summary(logrevfixed)






white(logrevfixed)
dwtest(logrevfixed)
coeftest(logrevfixed,vcovHC(logrevfixed, type = "HC1"))
#corrected for heteroskedasticity and serial correlation


resettest(logrevfixed)
#no specification error
vif(logrevfixed)
