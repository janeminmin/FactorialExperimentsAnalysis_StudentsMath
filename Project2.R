rm(list=ls())
## Load data
stuperf<-read.csv(file.choose(), header=TRUE, sep=',', col.names=c('gender','race','PLOE','lunch','TPC','mscore'))

## inspecting the balance of the design
str(stuperf)
table(stuperf$gender)
table(stuperf$race)
table(stuperf$PLOE)
table(stuperf$lunch)
table(stuperf$TPC)
replications(mscore ~ gender*race,stuperf)
replications(mscore ~ gender*PLOE,stuperf)
replications(mscore ~ gender*lunch,stuperf)
replications(mscore ~ gender*TPC,stuperf)
replications(mscore ~ race*PLOE,stuperf)
replications(mscore ~ race*lunch,stuperf)
replications(mscore ~ race*TPC,stuperf)
replications(mscore ~ PLOE*lunch,stuperf)
replications(mscore ~ PLOE*TPC,stuperf)
replications(mscore ~ lunch*TPC,stuperf)
##unbalanced, fix effect model

## data insight
boxplot(stuperf$mscore,col='grey')
##see outliers
qqnorm(stuperf$mscore)
qqline(stuperf$mscore,col='red')
hist(stuperf$mscore,prob = TRUE)
lines(density(stuperf$mscore),lwd = 2, col = "red")
#negative skew
library(ggplot2)
ggplot(stuperf,aes(x=mscore,group=gender,fill=gender))+
        geom_histogram(position="dodge",binwidth=0.25)+theme_bw()
ggplot(stuperf,aes(x=mscore,group=race,fill=race))+
        geom_histogram(position="dodge",binwidth=0.25)+theme_bw()
ggplot(stuperf,aes(x=mscore,group=PLOE,fill=PLOE))+
        geom_histogram(position="dodge",binwidth=0.25)+theme_bw()
ggplot(stuperf,aes(x=mscore,group=lunch,fill=lunch))+
        geom_histogram(position="dodge",binwidth=0.25)+theme_bw()
ggplot(stuperf,aes(x=mscore,group=TPC,fill=TPC))+
        geom_histogram(position="dodge",binwidth=0.25)+theme_bw()

##If we remove outliers
res <- boxplot(stuperf$mscore, outline=FALSE)
stuperf1<-stuperf[!stuperf$mscore %in% res$out, ]
##Check Normality after removing the outliers
qqnorm(stuperf1$mscore)
qqline(stuperf1$mscore,col='red')
hist(stuperf1$mscore,prob = TRUE)
lines(density(stuperf1$mscore),lwd = 2, col = "red")
##much better now



## inspecting the interaction
par(mfrow=c(1,2))
with(stuperf1,{
        interaction.plot(gender,race,mscore,type="b",legend="T",ylab="math score",
                         main="Interaction Plot",col=c("black","red","green"),pch=c(19,17,15))
        interaction.plot(gender,PLOE,mscore,type="b",legend="T",ylab="math score",
                         main="Interaction Plot",col=c("black","red","green"),pch=c(19,17,15))
        interaction.plot(gender,lunch,mscore,type="b",legend="T",ylab="math score",
                         main="Interaction Plot",col=c("black","red","green"),pch=c(19,17,15))
        interaction.plot(gender,TPC,mscore,type="b",legend="T",ylab="math score", 
                         main="Interaction Plot",col=c("black","red","green"),pch=c(19,17,15))
        interaction.plot(race,PLOE,mscore,type="b",legend="T",ylab="math score", 
                         main="Interaction Plot",col=c("black","red","green"),pch=c(19,17,15))
        interaction.plot(race,lunch,mscore,type="b",legend="T",ylab="math score", 
                         main="Interaction Plot",col=c("black","red","green"),pch=c(19,17,15))
        interaction.plot(race,TPC,mscore,type="b",legend="T",ylab="math score",
                         main="Interaction Plot",col=c("black","red","green"),pch=c(19,17,15))
        interaction.plot(PLOE,lunch,mscore,type="b",legend="T",ylab="math score", 
                         main="Interaction Plot",col=c("black","red","green"),pch=c(19,17,15))
        interaction.plot(PLOE,TPC,mscore,type="b",legend="T",ylab="math score", 
                         main="Interaction Plot",col=c("black","red","green"),pch=c(19,17,15))
        interaction.plot(lunch,TPC,mscore,type="b",legend="T",ylab="math score", 
                         main="Interaction Plot",col=c("black","red","green"),pch=c(19,17,15))
        
})
## find interation between PLOE & race, PLOE & gender, gender & race


## Anova analysis
library(car)
#Anova(lm(mscore ~ gender * race * PLOE * lunch * TPC, data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
#we can not launch the test with anova, since we do not have enough data (at least one cell having no data)
Anova(lm(mscore ~ gender * race, data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
Anova(lm(mscore ~ gender * PLOE, data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
Anova(lm(mscore ~ gender * lunch, data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
Anova(lm(mscore ~ gender * TPC, data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
Anova(lm(mscore ~ race * PLOE, data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
Anova(lm(mscore ~ race * lunch, data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
Anova(lm(mscore ~ race * TPC , data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
Anova(lm(mscore ~ PLOE * lunch, data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
Anova(lm(mscore ~ PLOE * TPC, data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
Anova(lm(mscore ~ lunch * TPC, data=stuperf1, contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
# no significant interaction found


# we also tried to Split data into subgroup to take 3-way anova analysis, 
# overlap factors gender * TPC or gender *lunch or TPC *lunch
# neither of them has enough data for anova.

# Now we may confirm that there is no interaction among these 5 factors.
# To simplyfy the case, we use main effect without interaction anova model
Anova(lm(mscore ~ gender + race + PLOE + lunch + TPC, data=stuperf1, 
         contrasts=list(topic=contr.sum, sys=contr.sum)), type=3)
# Each factor has a significant effect

## check the validity of anova
## https://www.theanalysisfactor.com/checking-normality-anova-model/
##Check Anova test assumptions
options(contrasts = c("contr.sum","contr.poly"))
model <- lm(mscore ~gender + race + PLOE + lunch + TPC, data=stuperf1)
Anova(model,type=3)
## Check Univariate Normality
shapiro.test(model$residuals)
## unvalide anova, violation of normality


## therefore we launch the one-way anova for each factor
boxplot(mscore ~ gender, data=stuperf1, ylab="math score", xlab="gender",col=c('mistyrose','powderblue'))
model1<-lm(mscore ~ gender, data=stuperf1)
Anova(model1,type=3)
## 1.Check Univariate Normality
shapiro.test(model1$residuals)
##2.Check homogeneity of variances
bartlett.test(mscore ~ gender,data=stuperf1)
par(mfrow=c(2,2))
plot(model1)

boxplot(mscore ~ race, data=stuperf1, notch=TRUE, ylab="math score", xlab="race",col=rainbow(length(unique(stuperf1$race))))
model2<-lm(mscore ~ race, data=stuperf1)
Anova(model2,type=3)
shapiro.test(model2$residuals)
bartlett.test(mscore ~ race,data=stuperf1)
par(mfrow=c(2,2))
plot(model2)

model3<-lm(mscore ~ PLOE, data=stuperf1)
Anova(model3,type=3)
shapiro.test(model3$residuals)
bartlett.test(mscore ~ PLOE,data=stuperf1)
#unvalide anova, violation of normality

boxplot(mscore ~ lunch, data=stuperf1, ylab="math score", xlab="lunch",col=c('grey','lightpink2'))
model4<-lm(mscore ~ lunch, data=stuperf1)
Anova(model4,type=3)
shapiro.test(model4$residuals)
bartlett.test(mscore ~ lunch,data=stuperf1)
par(mfrow=c(2,2))
plot(model4)


model5<-lm(mscore ~ TPC, data=stuperf1)
Anova(model5,type=3)
shapiro.test(model5$residuals)
bartlett.test(mscore ~ TPC,data=stuperf1)
#unvalide anova, violation of normality



## post hoc tests:All three factors:gender, race, lunch have passed the bartlett.test, equal variance anova use Turkey test
## unbalanced data.https://rcompanion.org/rcompanion/d_05.html here we use lsmeans package

library(lsmeans)
ls_g<-lsmeans(model1,pairwise ~ gender, adjust = "tukey")
plot(ls_g)


ls_r<-lsmeans(model2,pairwise ~ race, adjust = "tukey")
plot(ls_r)

ls_l<-lsmeans(model4,pairwise ~ lunch, adjust = "tukey")
plot(ls_l)

## Wilcoxon rank sum test for TPC
##install.packages("ggpubr")
#Plot mscore by TPC
library(ggpubr)
ggboxplot(stuperf1,x="TPC", y="mscore",
          color="TPC",palette=c("#00AFBB","#E7B800"),
          ylab="mscore",xlab="TPC")

wilcox.test(mscore~TPC, stuperf1, exact=FALSE)
## install.packages("dplyr")
library(dplyr)
group_by(stuperf1,TPC) %>%
        summarise(count=n(),
                  median=median(mscore),
                  mean=mean(mscore))
##Kruskal-Wallis test for PLOE
#Plot mscore by PLOE
ggboxplot(stuperf1,x="PLOE", y="mscore",
          color="PLOE",palette=c("#B2182B", "#F4A582", "#00AFBB", "#E7B800", "#92C5DE","#2166AC"),
          order=c("some high school", "high school", "associate's degree", 
                  "some college", "bachelor's degree", "master's degree"),
          ylab="mscore",xlab="PLOE")
kruskal.test(mscore~PLOE,stuperf1)
pairwise.wilcox.test(stuperf1$mscore,stuperf1$PLOE,p.adjust.method="BH")
group_by(stuperf1,PLOE) %>%
        summarise(count=n(),
                  median=median(mscore),
                  mean=mean(mscore))

