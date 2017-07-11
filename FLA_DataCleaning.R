#####################################################################################
############## Data Cleaning File for Foreign Language Anxiety Project ##############
#####################################################################################

### Import Japanese Data ###
KGUDT <- read.csv('_data/KBSASP2017.csv', header = T)

### Import US Data ###
UHMDT <- read.csv('_data/SASP2017.csv', header = T)
UHMDT$ResponseID <- as.factor(UHMDT$id)

### Code Ethnicity and Generation ###
library(car)
UHMDT[,c(5:25,27:33)]<-apply(UHMDT[,c(5:25,27:33)],2, function(x) { #CHANGE TO 32 WHEN FINAL USED
  x<-recode(x, "1=1; NA=0")
})

UHMDT$race.asian <-(as.numeric(UHMDT$ethnicity.Japanese)+as.numeric(UHMDT$ethnicity.Chinese)+
                    as.numeric(UHMDT$ethnicity.Korean)+as.numeric(UHMDT$ethnicity.Okinawan)+
                    as.numeric(UHMDT$ethnicity.Vietnamese)+as.numeric(UHMDT$ethnicity.Laotian)+
                    as.numeric(UHMDT$ethnicity.Thai))
UHMDT$race.white <-(as.numeric(UHMDT$ethnicity.Caucasian.White)+as.numeric(UHMDT$ethnicity.Portuguese)+
                    as.numeric(UHMDT$ethnicity.Jewish))
UHMDT$race.latino <-(as.numeric(UHMDT$ethnicity.Hispanic.Latino)+as.numeric(UHMDT$ethnicity.Puerto.Rican))
UHMDT$race.other <-(as.numeric(UHMDT$ethnicity.Fillipino)+as.numeric(UHMDT$ethnicity.Samoan)+
                    as.numeric(UHMDT$ethnicity.Native.Hawaiian)+as.numeric(UHMDT$ethnicity.African.American.Black)+
                    as.numeric(UHMDT$ethnicity.Tongan)+as.numeric(UHMDT$ethnicity.Asian.Indian)+
                    as.numeric(UHMDT$ethnicity.Native.American.Alaska.Native)+as.numeric(UHMDT$ethnicity.Malaysian)+
                    as.numeric(UHMDT$ethnicity.Javanese)+as.numeric(UHMDT$ethnicity.Marshallese)+
                    as.numeric(UHMDT$ethnicity.Palauan)+as.numeric(UHMDT$ethnicity.Tahitian)+
                    as.numeric(UHMDT$ethnicity.Fijian)+as.numeric(UHMDT$ethnicity.Guamanian.or.Chamorro)+
                    as.numeric(UHMDT$ethnicity.Micronesian..Chuukese..Kosraean..Pohnpeian..Yapese.))

UHMDT$race.asian<-recode(UHMDT$race.asian, "1:10 = 2; 0 = 0")
UHMDT$race.white<-recode(UHMDT$race.white, "1:10 = 1; 0 = 0")
UHMDT$race.latino<-recode(UHMDT$race.latino, "1:10 = 4; 0 = 0")
UHMDT$race.other<-recode(UHMDT$race.other, "1:10 = 10; 0 = 0")

UHMDT$group <-(UHMDT$race.asian+UHMDT$race.white+UHMDT$race.latino+UHMDT$race.other)
UHMDT<-rbind(subset(UHMDT, group == 1),subset(UHMDT, group == 2),subset(UHMDT, group == 3),
             subset(UHMDT, group == 4), subset(UHMDT, group ==5))
UHMDT$group<-as.factor(recode(UHMDT$group, "1 = 'EH'; 2 = 'AH'; 3 = 'BR'; 4 = 'LT'; 5 = 'OT'"))

#Code generation
UHMDT$generation <- UHMDT$birthplace + UHMDT$mother.birthplace + UHMDT$father.birthplace
UHMDT$generation <- recode(UHMDT$generation, "1 = 1; 2 = 2; 3 = 3; 4 = 3; 5 = 3")

#Delete Race and Generation Columns
library(data.table)
UHMDT<-as.data.frame(setDT(UHMDT)[,c("race.asian","race.white","race.latino","race.other",
                                     "ethnicity.Japanese","ethnicity.Chinese","ethnicity.Korean",
                                     "ethnicity.Fillipino","ethnicity.Samoan","ethnicity.Caucasian.White",
                                     "ethnicity.Portuguese","ethnicity.Vietnamese","ethnicity.Native.Hawaiian",
                                     "ethnicity.African.American.Black","ethnicity.Hispanic.Latino",
                                     "ethnicity.Laotian","ethnicity.Tongan","ethnicity.Puerto.Rican",
                                     "ethnicity.Asian.Indian","ethnicity.Native.American.Alaska.Native",
                                     "ethnicity.Thai","ethnicity.Malaysian","ethnicity.Javanese",
                                     "ethnicity.Jewish","ethnicity.Other","ethnicity.Marshallese",
                                     "ethnicity.Palauan","ethnicity.Tahitian","ethnicity.Other.TEXT",                               
                                     "ethnicity.Fijian","ethnicity.Okinawan","birthplace","mother.birthplace",
                                     "father.birthplace","ethnicity.Guamanian.or.Chamorro","id",
                                     "ethnicity.Micronesian..Chuukese..Kosraean..Pohnpeian..Yapese."):=NULL])

#Add a sample variable
UHMDT$sample <- rep("UHM", nrow(UHMDT))

### Combine Data ###
cols <- intersect(colnames(KGUDT), colnames(UHMDT))
FULLDT <- rbind(KGUDT[cols], UHMDT[,cols])
LANGDT <-FULLDT[FULLDT$foreign.language == 1 & FULLDT$current.class == 1,]
rm(cols)

### Multiple Imputation ###
set.seed(3225)
library(mice)
imp<-mice(FULLDT[,-c(1)],defaultMethod = c("pmm","logreg","polyreg"))
FULLDT[,-c(1)]<-complete(imp)

imp<-mice(LANGDT[,-c(1)],defaultMethod = c("pmm","logreg","polyreg"))
LANGDT[,-c(1)]<-complete(imp)

imp<-mice(KGUDT[,-c(1,8,12,18,126,127)],defaultMethod = c("pmm","logreg","polyreg"))
KGUDT[,-c(1,8,12,18,126,127)]<-complete(imp)

#Remove imputation file
rm(imp)

#Save/load imputed data
#save.image('imputedDT.RData')
load('imputedDT.RData')

### Alphas & Scale Construction ###
library(psych)

#SPS and SIAS
alpha(FULLDT[FULLDT$sample == "KGU", 51:56])$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", 51:56])$total$raw_alpha #UHM sample
FULLDT$sps6  <- (FULLDT$SPS.4+FULLDT$SPS.7+FULLDT$SPS.8+FULLDT$SPS.15+FULLDT$SPS.16+FULLDT$SPS.17)
LANGDT$sps6  <- (LANGDT$SPS.4+LANGDT$SPS.7+LANGDT$SPS.8+LANGDT$SPS.15+LANGDT$SPS.16+LANGDT$SPS.17)
KGUDT$sps6  <- (KGUDT$SPS.4+KGUDT$SPS.7+KGUDT$SPS.8+KGUDT$SPS.15+KGUDT$SPS.16+KGUDT$SPS.17)

alpha(FULLDT[FULLDT$sample == "KGU", 57:62])$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", 57:62])$total$raw_alpha #UHM sample
FULLDT$sias6 <- (FULLDT$SIAS.2+FULLDT$SIAS.4+FULLDT$SIAS.6+FULLDT$SIAS.8+FULLDT$SIAS.10+FULLDT$SIAS.13)
LANGDT$sias6 <- (LANGDT$SIAS.2+LANGDT$SIAS.4+LANGDT$SIAS.6+LANGDT$SIAS.8+LANGDT$SIAS.10+LANGDT$SIAS.13)
KGUDT$sias6 <- (KGUDT$SIAS.2+KGUDT$SIAS.4+KGUDT$SIAS.6+KGUDT$SIAS.8+KGUDT$SIAS.10+KGUDT$SIAS.13)

#New Self-Construal
alpha(FULLDT[FULLDT$sample == "KGU", 63:67])$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", 63:67])$total$raw_alpha #UHM sample
FULLDT$rejAvoid  <- (FULLDT$RA.1+FULLDT$RA.2+FULLDT$RA.3+FULLDT$RA.4+FULLDT$RA.5)
LANGDT$rejAvoid  <- (LANGDT$RA.1+LANGDT$RA.2+LANGDT$RA.3+LANGDT$RA.4+LANGDT$RA.5)
KGUDT$rejAvoid  <- (KGUDT$RA.1+KGUDT$RA.2+KGUDT$RA.3+KGUDT$RA.4+KGUDT$RA.5)

alpha(FULLDT[FULLDT$sample == "KGU", 68:72])$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", 68:72])$total$raw_alpha #UHM sample
FULLDT$selfExp   <- (FULLDT$SE.1+FULLDT$SE.2+FULLDT$SE.3+FULLDT$SE.4+FULLDT$SE.5)
LANGDT$selfExp   <- (LANGDT$SE.1+LANGDT$SE.2+LANGDT$SE.3+LANGDT$SE.4+LANGDT$SE.5)
KGUDT$selfExp   <- (KGUDT$SE.1+KGUDT$SE.2+KGUDT$SE.3+KGUDT$SE.4+KGUDT$SE.5)

alpha(FULLDT[FULLDT$sample == "KGU", 73:77])$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", 73:77])$total$raw_alpha #UHM sample
FULLDT$harmSeek  <- (FULLDT$HS.1+FULLDT$HS.2+FULLDT$HS.3+FULLDT$HS.4+FULLDT$HS.5)
LANGDT$harmSeek  <- (LANGDT$HS.1+LANGDT$HS.2+LANGDT$HS.3+LANGDT$HS.4+LANGDT$HS.5)
KGUDT$harmSeek  <- (KGUDT$HS.1+KGUDT$HS.2+KGUDT$HS.3+KGUDT$HS.4+KGUDT$HS.5)

alpha(FULLDT[FULLDT$sample == "KGU", 78:82])$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", 78:82])$total$raw_alpha #UHM sample
FULLDT$distSelf  <- (FULLDT$DS.1+FULLDT$DS.2+FULLDT$DS.3+FULLDT$DS.4+FULLDT$DS.5)
LANGDT$distSelf  <- (LANGDT$DS.1+LANGDT$DS.2+LANGDT$DS.3+LANGDT$DS.4+LANGDT$DS.5)
KGUDT$distSelf  <- (KGUDT$DS.1+KGUDT$DS.2+KGUDT$DS.3+KGUDT$DS.4+KGUDT$DS.5)

#Relational Mobility
alpha(FULLDT[FULLDT$sample == "KGU", 78:82])$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", 78:82])$total$raw_alpha #UHM sample
FULLDT$relatMob <- (FULLDT$RM.1+FULLDT$RM.2+FULLDT$RM.3+FULLDT$RM.4+FULLDT$RM.5+FULLDT$RM.6+
                    FULLDT$RM.7+FULLDT$RM.8+FULLDT$RM.9+FULLDT$RM.10+FULLDT$RM.11+FULLDT$RM.12)
LANGDT$relatMob <- (LANGDT$RM.1+LANGDT$RM.2+LANGDT$RM.3+LANGDT$RM.4+LANGDT$RM.5+LANGDT$RM.6+
                    LANGDT$RM.7+LANGDT$RM.8+LANGDT$RM.9+LANGDT$RM.10+LANGDT$RM.11+LANGDT$RM.12)
KGUDT$relatMob <- (KGUDT$RM.1+KGUDT$RM.2+KGUDT$RM.3+KGUDT$RM.4+KGUDT$RM.5+KGUDT$RM.6+
                   KGUDT$RM.7+KGUDT$RM.8+KGUDT$RM.9+KGUDT$RM.10+KGUDT$RM.11+KGUDT$RM.12)

#Intergroup Anxiety
alpha(FULLDT[FULLDT$sample == "KGU", 95:105],check.keys=TRUE)$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", 95:105],check.keys=TRUE)$total$raw_alpha #UHM sample

FULLDT$IA.5 <- recode(FULLDT$IA.5,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$IA.6 <- recode(FULLDT$IA.6,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$IA.8 <- recode(FULLDT$IA.8,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$interAnx <- (FULLDT$IA.1+FULLDT$IA.2+FULLDT$IA.3+FULLDT$IA.4+FULLDT$IA.5+FULLDT$IA.6+
                    FULLDT$IA.7+FULLDT$IA.8+FULLDT$IA.9+FULLDT$IA.10+FULLDT$IA.11)

LANGDT$IA.5 <- recode(LANGDT$IA.5,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$IA.6 <- recode(LANGDT$IA.6,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$IA.8 <- recode(LANGDT$IA.8,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$interAnx <- (LANGDT$IA.1+LANGDT$IA.2+LANGDT$IA.3+LANGDT$IA.4+LANGDT$IA.5+LANGDT$IA.6+
                    LANGDT$IA.7+LANGDT$IA.8+LANGDT$IA.9+LANGDT$IA.10+LANGDT$IA.11)

KGUDT$IA.5 <- recode(KGUDT$IA.5,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$IA.6 <- recode(KGUDT$IA.6,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$IA.8 <- recode(KGUDT$IA.8,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$interAnx <- (KGUDT$IA.1+KGUDT$IA.2+KGUDT$IA.3+KGUDT$IA.4+KGUDT$IA.5+KGUDT$IA.6+
                   KGUDT$IA.7+KGUDT$IA.8+KGUDT$IA.9+KGUDT$IA.10+KGUDT$IA.11)

#Distress Tolerance
alpha(FULLDT[FULLDT$sample == "KGU", 106:120],check.keys=TRUE)$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", 106:120],check.keys=TRUE)$total$raw_alpha #UHM sample

FULLDT$DT.6 <- recode(FULLDT$DT.5,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$distTol <- (FULLDT$DT.1+FULLDT$DT.2+FULLDT$DT.3+FULLDT$DT.4+FULLDT$DT.5+FULLDT$DT.6+
                      FULLDT$DT.7+FULLDT$DT.8+FULLDT$DT.9+FULLDT$DT.10+FULLDT$DT.11+
                      FULLDT$DT.12+FULLDT$DT.13+FULLDT$DT.14+FULLDT$DT.15)

LANGDT$DT.6 <- recode(LANGDT$DT.5,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$distTol <- (LANGDT$DT.1+LANGDT$DT.2+LANGDT$DT.3+LANGDT$DT.4+LANGDT$DT.5+LANGDT$DT.6+
                     LANGDT$DT.7+LANGDT$DT.8+LANGDT$DT.9+LANGDT$DT.10+LANGDT$DT.11+
                     LANGDT$DT.12+LANGDT$DT.13+LANGDT$DT.14+LANGDT$DT.15)

KGUDT$DT.6 <- recode(KGUDT$DT.5,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$distTol <- (KGUDT$DT.1+KGUDT$DT.2+KGUDT$DT.3+KGUDT$DT.4+KGUDT$DT.5+KGUDT$DT.6+
                     KGUDT$DT.7+KGUDT$DT.8+KGUDT$DT.9+KGUDT$DT.10+KGUDT$DT.11+
                     KGUDT$DT.12+KGUDT$DT.13+KGUDT$DT.14+KGUDT$DT.15)

#General good feelings
alpha(FULLDT[FULLDT$sample == "KGU", 121:124],check.keys=TRUE)$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", 121:124],check.keys=TRUE)$total$raw_alpha #UHM sample

FULLDT$ggf <- (FULLDT$ggf.1+FULLDT$ggf.2+FULLDT$ggf.3+FULLDT$ggf.4)
LANGDT$ggf <- (LANGDT$ggf.1+LANGDT$ggf.2+LANGDT$ggf.3+LANGDT$ggf.4)
KGUDT$ggf <- (KGUDT$ggf.1+KGUDT$ggf.2+KGUDT$ggf.3+KGUDT$ggf.4)

## FLCAS (Foreign Language Classroom Anxiety Scale) ##

#Recode
FULLDT$FLACS.2 <- recode(FULLDT$FLACS.2,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$FLACS.5 <- recode(FULLDT$FLACS.5,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$FLACS.8 <- recode(FULLDT$FLACS.8,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$FLACS.11 <- recode(FULLDT$FLACS.11,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$FLACS.14 <- recode(FULLDT$FLACS.14,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$FLACS.18 <- recode(FULLDT$FLACS.18,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$FLACS.22 <- recode(FULLDT$FLACS.22,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$FLACS.28 <- recode(FULLDT$FLACS.28,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
FULLDT$FLACS.32 <- recode(FULLDT$FLACS.32,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")

LANGDT$FLACS.2 <- recode(LANGDT$FLACS.2,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$FLACS.5 <- recode(LANGDT$FLACS.5,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$FLACS.8 <- recode(LANGDT$FLACS.8,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$FLACS.11 <- recode(LANGDT$FLACS.11,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$FLACS.14 <- recode(LANGDT$FLACS.14,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$FLACS.18 <- recode(LANGDT$FLACS.18,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$FLACS.22 <- recode(LANGDT$FLACS.22,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$FLACS.28 <- recode(LANGDT$FLACS.28,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
LANGDT$FLACS.32 <- recode(LANGDT$FLACS.32,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")

KGUDT$FLACS.2 <- recode(KGUDT$FLACS.2,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$FLACS.5 <- recode(KGUDT$FLACS.5,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$FLACS.8 <- recode(KGUDT$FLACS.8,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$FLACS.11 <- recode(KGUDT$FLACS.11,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$FLACS.14 <- recode(KGUDT$FLACS.14,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$FLACS.18 <- recode(KGUDT$FLACS.18,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$FLACS.22 <- recode(KGUDT$FLACS.22,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$FLACS.28 <- recode(KGUDT$FLACS.28,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")
KGUDT$FLACS.32 <- recode(KGUDT$FLACS.32,"1 = 7; 2 = 6; 3 = 5; 4 = 4; 5 = 3; 6 = 2; 7 = 1")

#Test Anxiety
colN <- c("FLCAS.3","FLCAS.5","FLCAS.6","FLCAS.8","FLCAS.10","FLCAS.11","FLCAS.12","FLCAS.16",
          "FLCAS.17","FLCAS.20","FLCAS.21","FLCAS.22","FLCAS.25","FLCAS.26","FLCAS.28")
alpha(FULLDT[FULLDT$sample == "KGU", colN],check.keys=TRUE)$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", colN],check.keys=TRUE)$total$raw_alpha #UHM sample

FULLDT$testAnx <-(FULLDT$FLCAS.3+FULLDT$FLCAS.5+FULLDT$FLCAS.6+FULLDT$FLCAS.8+FULLDT$FLCAS.10+
                  FULLDT$FLCAS.11+FULLDT$FLCAS.12+FULLDT$FLCAS.16+FULLDT$FLCAS.17+FULLDT$FLCAS.20+
                  FULLDT$FLCAS.21+FULLDT$FLCAS.22+FULLDT$FLCAS.25+FULLDT$FLCAS.26+FULLDT$FLCAS.28)
LANGDT$testAnx <-(LANGDT$FLCAS.3+LANGDT$FLCAS.5+LANGDT$FLCAS.6+LANGDT$FLCAS.8+LANGDT$FLCAS.10+
                  LANGDT$FLCAS.11+LANGDT$FLCAS.12+LANGDT$FLCAS.16+LANGDT$FLCAS.17+LANGDT$FLCAS.20+
                  LANGDT$FLCAS.21+LANGDT$FLCAS.22+LANGDT$FLCAS.25+LANGDT$FLCAS.26+LANGDT$FLCAS.28)
KGUDT$testAnx <-(KGUDT$FLCAS.3+KGUDT$FLCAS.5+KGUDT$FLCAS.6+KGUDT$FLCAS.8+KGUDT$FLCAS.10+
                  KGUDT$FLCAS.11+KGUDT$FLCAS.12+KGUDT$FLCAS.16+KGUDT$FLCAS.17+KGUDT$FLCAS.20+
                  KGUDT$FLCAS.21+KGUDT$FLCAS.22+KGUDT$FLCAS.25+KGUDT$FLCAS.26+KGUDT$FLCAS.28)

#Negative Evaluation
colN <- c("FLCAS.2","FLCAS.7","FLCAS.13","FLCAS.19","FLCAS.23","FLCAS.31","FLCAS.33")
alpha(FULLDT[FULLDT$sample == "KGU", colN],check.keys=TRUE)$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", colN],check.keys=TRUE)$total$raw_alpha #UHM sample

FULLDT$negEval <-(FULLDT$FLCAS.2+FULLDT$FLCAS.7+FULLDT$FLCAS.13+FULLDT$FLCAS.19+FULLDT$FLCAS.23+
                  FULLDT$FLCAS.31+FULLDT$FLCAS.33)
LANGDT$negEval <-(LANGDT$FLCAS.2+LANGDT$FLCAS.7+LANGDT$FLCAS.13+LANGDT$FLCAS.19+LANGDT$FLCAS.23+
                  LANGDT$FLCAS.31+LANGDT$FLCAS.33)
KGUDT$negEval <-(KGUDT$FLCAS.2+KGUDT$FLCAS.7+KGUDT$FLCAS.13+KGUDT$FLCAS.19+KGUDT$FLCAS.23+
                  KGUDT$FLCAS.31+KGUDT$FLCAS.33)

#Communication Apprehension
colN <- c("FLCAS.1","FLCAS.4","FLCAS.9","FLCAS.14","FLCAS.15","FLCAS.18","FLCAS.24",
          "FLCAS.27","FLCAS.29","FLCAS.30","FLCAS.32")
alpha(FULLDT[FULLDT$sample == "KGU", colN],check.keys=TRUE)$total$raw_alpha #KGU sample
alpha(FULLDT[FULLDT$sample == "UHM", colN],check.keys=TRUE)$total$raw_alpha #UHM sample

FULLDT$commApp <-(FULLDT$FLCAS.1+FULLDT$FLCAS.4+FULLDT$FLCAS.9+FULLDT$FLCAS.14+FULLDT$FLCAS.15+
                  FULLDT$FLCAS.18+FULLDT$FLCAS.24+FULLDT$FLCAS.27+FULLDT$FLCAS.29+FULLDT$FLCAS.30+
                  FULLDT$FLCAS.32)
LANGDT$commApp <-(LANGDT$FLCAS.1+LANGDT$FLCAS.4+LANGDT$FLCAS.9+LANGDT$FLCAS.14+LANGDT$FLCAS.15+
                  LANGDT$FLCAS.18+LANGDT$FLCAS.24+LANGDT$FLCAS.27+LANGDT$FLCAS.29+LANGDT$FLCAS.30+
                  LANGDT$FLCAS.32)
KGUDT$commApp <-(KGUDT$FLCAS.1+KGUDT$FLCAS.4+KGUDT$FLCAS.9+KGUDT$FLCAS.14+KGUDT$FLCAS.15+
                  KGUDT$FLCAS.18+KGUDT$FLCAS.24+KGUDT$FLCAS.27+KGUDT$FLCAS.29+KGUDT$FLCAS.30+
                  KGUDT$FLCAS.32)

#Construct brief dataset
BRIEFDT <- subset(FULLDT, select = c(1:17,125:136))
rm(colN)

### Fix KGU Names ###

#Name case function
firstup <- function(x) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  return(x)
}

#Lowercase
KGUDT$firstname<-sapply(KGUDT$firstname, tolower)
KGUDT$firstname<-sapply(KGUDT$firstname, firstup)
KGUDT$lastname<-sapply(KGUDT$lastname, tolower)
KGUDT$lastname<-sapply(KGUDT$lastname, firstup)

checkname<-paste(KGUDT$firstname,KGUDT$lastname, sep = "")
table(checkname)

rm(checkname, firstup)

#Save/Load Rdata
#save.image('cleanedData.RData')
