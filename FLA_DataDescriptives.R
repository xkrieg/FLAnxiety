#####################################################################################
############ Data Descriptives File for Foreign Language Anxiety Project ############
#####################################################################################

#Load data
load('cleanedData.RData')

### Categorical Variables ###
#Sample size
table(BRIEFDT$sample)

#Gender distribution
table(BRIEFDT$gender, BRIEFDT$sample)

#Ethnic Group (UHM only)
table(BRIEFDT[BRIEFDT$sample == "UHM", "group"])

#Generation status by ethnicity (UHM only)
table(BRIEFDT[BRIEFDT$sample == "UHM", "generation"],
      BRIEFDT[BRIEFDT$sample == "UHM", "group"])

### Numeric Variables ###
library(psych)
x<-describe(BRIEFDT[,c(3:4,9:26)])

#Means and SD by sample
library(doBy)
numNames <- names(BRIEFDT[,c(3:4,9:29)])

for (i in 1:length(numNames)){
  print(numNames[i])
  print(summaryBy(BRIEFDT[,numNames[i]] ~ BRIEFDT$sample, data = BRIEFDT, FUN = c(mean,sd)))
}

#Mean Difference?
BRIEFDT <- within(BRIEFDT, group <- relevel(as.factor(group), ref = "EH"))
BRIEFDT <- within(BRIEFDT, sample <- relevel(as.factor(sample), ref = "UHM"))

#By sample
for (i in 1:length(numNames)){
  print(numNames[i])
  print(summary(lm(BRIEFDT[,numNames[i]] ~ sample, BRIEFDT)))
}

#By ethnic group
for (i in 1:length(numNames)){
  print(numNames[i])
  print(summary(lm(BRIEFDT[,numNames[i]] ~ group, BRIEFDT)))
}

#Predictor cor matrix
cor(BRIEFDT[,c(19,25,27:29)])

#Linear modeling
m1 <-lm(cbind(proficiency,language.confidence,language.week) ~ gender+age+years.language_1,
        data = BRIEFDT); summary(m1)
m2 <-lm(cbind(proficiency,language.confidence,language.week) ~ sias6+gender+age+years.language_1, 
        data = BRIEFDT); summary(m2)
m3 <-lm(cbind(proficiency,language.confidence,language.week) ~ 
        commApp+negEval+testAnx+interAnx+sias6+gender+age+years.language_1, 
        data = BRIEFDT); summary(m3)

m4 <-lm(cbind(proficiency,language.confidence,language.week) ~ commApp+negEval+testAnx+interAnx+sias6, 
        data = BRIEFDT); summary(m4)

anova(m1,m2,m3)

m1 <-lm(cbind(proficiency,language.confidence,language.week) ~ gender+age+years.language_1,
        data = LANGDT); summary(m1)
m2 <-lm(cbind(proficiency,language.confidence,language.week) ~ sias6+gender+age+years.language_1, 
        data = LANGDT); summary(m2)
m3 <-lm(cbind(proficiency,language.confidence,language.week) ~ 
          commApp+negEval+interAnx+sias6+gender+age+years.language_1, 
        data = LANGDT); summary(m3)

anova(m1,m2,m3)

m1 <-lm(cbind(proficiency,language.confidence,language.week) ~ gender+age+years.language_1,
        data = KGUDT); summary(m1)
m2 <-lm(cbind(proficiency,language.confidence,language.week) ~ sias6+gender+age+years.language_1, 
        data = KGUDT); summary(m2)
m3 <-lm(cbind(proficiency,language.confidence,language.week) ~ 
          commApp+negEval+interAnx+sias6+gender+age+years.language_1, 
        data = KGUDT); summary(m3)

anova(m1,m2,m3)
