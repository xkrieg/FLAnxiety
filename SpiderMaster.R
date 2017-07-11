##### Spider Plot Master File #####

#Import Libraries
library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)

#Load Some Data
load('cleanedData.RData')

#Get IDs
nameDT<-KGUDT[KGUDT$lastname != "", c("ResponseID","firstname","lastname")]

df_spider <-as.data.frame(cbind(pnorm(scale(BRIEFDT[,c(19,25,27:29)]))))
df_spider$ID <-as.character(BRIEFDT[,"ResponseID"])
df_spider <-subset(df_spider, select = c("ID","sias6","interAnx","testAnx","negEval","commApp"))
names(df_spider) <- c("ID","Social Anxiety", "Intergroup\nAnxiety", "Test Anxiety",
                      "Classmates\'\nJudgment","Communicate\nApprehension")

#Critical score meta
critical <-data.frame(id=c(0),number=c(0))

for (i in 1:nrow(df_spider)){
  
  #Case Number
  print(i)
  if (df_spider[i, "ID"] %in% nameDT$ResponseID){
    print("OKAY")
    case_spider <- df_spider[i,]
    case_spider[2,] <-c("Average",
                    mean(df_spider[df_spider$ID %in% nameDT$ResponseID, "Social Anxiety"]),
                    mean(df_spider[df_spider$ID %in% nameDT$ResponseID, "Intergroup\nAnxiety"]),
                    mean(df_spider[df_spider$ID %in% nameDT$ResponseID, "Test Anxiety"]),
                    mean(df_spider[df_spider$ID %in% nameDT$ResponseID, "Classmates\'\nJudgment"]),
                    mean(df_spider[df_spider$ID %in% nameDT$ResponseID, "Communicate\nApprehension"]))
  
    case_spider[,2:6] <-sapply(case_spider[,2:6], as.numeric)
    
    #Strengths and Weaknesses
    factornames <- c("Social Anxiety", "Intergroup Anxiety", "Test Anxiety",
                     "Classmates\' Judgment","Communicate Apprehension")
    strengths <- c()
    weakness <- c()
  
    for (j in 2:length(case_spider)){
      if (case_spider[1,j] > .65){
        weakness <- append(weakness, factornames[j-1])
      }
      if (case_spider[1,j] < .4){
        strengths <- append(strengths, factornames[j-1])
      }
    }
    
    #Save No. of Weaknesses
    critical[nrow(critical)+1,] <-c(df_spider[i,"ID"],length(weakness)) 
    
    if (length(weakness) == 5){
      textboxtext_w <- paste("*", weakness[1],"*, *", weakness[2],"*, *", weakness[3],
                             "*, *",weakness[4],"*, and *", weakness[5], "*", sep = "")
    }
    if (length(weakness) == 4){
      textboxtext_w <- paste("*", weakness[1],"*, *", weakness[2],"*, *", weakness[3],
                             "*, and *", weakness[4], "*", sep = "")
    }
    if (length(weakness) == 3){
      textboxtext_w <- paste("*", weakness[1],"*, *", weakness[2],"*, and *",
                             weakness[3], "*", sep = "")
    }
    if (length(weakness) == 2){
      textboxtext_w <- paste("*", weakness[1],"* and *", weakness[2], "*", sep = "")
    }
    if (length(weakness) == 1){
      textboxtext_w <- paste("*", weakness[1], "*", sep = "")
    }
    if (length(weakness) == 0){
      textboxtext_w <- "something that you struggle with"
    }
  
    if (length(strengths) == 5){
      textboxtext_s <- paste("*", strengths[1],"*, *", strengths[2],"*, *", strengths[3],
                             "*, *",strengths[4],"*, and *", strengths[5], "*", sep = "")
    }
    if (length(strengths) == 4){
      textboxtext_s <- paste("*", strengths[1],"*, *", strengths[2],"*, *", strengths[3],
                             "*, and *", strengths[4], "*", sep = "")
    }
    if (length(strengths) == 3){
      textboxtext_s <- paste("*", strengths[1],"*, *", strengths[2],"*, and *",
                             strengths[3], "*", sep = "")
    }
    if (length(strengths) == 2){
      textboxtext_s <- paste("*", strengths[1],"* and *", strengths[2], "*", sep = "")
    }
    if (length(strengths) == 1){
      textboxtext_s <- paste("*", strengths[1], "*", sep = "")
    }
    if (length(strengths) == 0){
      textboxtext_s <- "something that you do not struggle with"
    }
  
    #Names
    firstname = nameDT[as.character(nameDT$ResponseID) == as.character(df_spider[i, "ID"]), "firstname"]
    lastname = nameDT[as.character(nameDT$ResponseID) == as.character(df_spider[i, "ID"]), "lastname"]
    filename <- paste(lastname,firstname,"FLASReport", sep = "_")
  
    #Save workspace image for retrieval
    save.image()
  
    #Build HTMLs
    rmarkdown::render(input = 'SpiderReportMD_EN.Rmd', output_format = "html_document",
                      output_file = paste('_output/', filename, ".html", sep = ""))
  }
}

#Remove unnecessary variables before continuing
rm(filename, firstname, lastname, i, j, strengths, weakness,
   textboxtext_w, textboxtext_s)

################## Convert to PDF ####################
#Get File names
filenames <- list.files("_output", pattern="*.html", full.names=TRUE)

for (k in filenames){
  #Local command
  comm<-paste("wkhtmltopdf", k, paste(substr(k,1,nchar(k)-5),
                                ".pdf",sep = ""), sep = " ")
  #send command
  system(comm)
}

### Delete HTML ###
system(paste("rm",paste(filenames, collapse = " "), sep = " "))

### Meta-data ###
case_spider[2,2:6]*100

#Social Anxiety
length(df_spider[df_spider$ID %in% nameDT$ResponseID & df_spider[,"Social Anxiety"] > .65,"ID"])
length(df_spider[df_spider$ID %in% nameDT$ResponseID & df_spider[,"Social Anxiety"] > .65,
                 "ID"])/nrow(df_spider)*100

#Intergroup Anxiety
length(df_spider[df_spider$ID %in% nameDT$ResponseID & df_spider[,"Intergroup\nAnxiety"] > .65,"ID"])
length(df_spider[df_spider$ID %in% nameDT$ResponseID & df_spider[,"Intergroup\nAnxiety"] > .65,
                 "ID"])/nrow(df_spider)*100

#Test Anxiety
length(df_spider[df_spider$ID %in% nameDT$ResponseID & df_spider[,"Test Anxiety"] > .65,"ID"])
length(df_spider[df_spider$ID %in% nameDT$ResponseID & df_spider[,"Test Anxiety"] > .65,
                 "ID"])/nrow(df_spider)*100

#Classmate's Judgment
length(df_spider[df_spider$ID %in% nameDT$ResponseID & df_spider[,"Classmates\'\nJudgment"] > .60,"ID"])
length(df_spider[df_spider$ID %in% nameDT$ResponseID & df_spider[,"Classmates\'\nJudgment"] > .60,
                 "ID"])/nrow(df_spider)*100

#Communication Apprehension
length(df_spider[df_spider$ID %in% nameDT$ResponseID & df_spider[,"Communicate\nApprehension"] > .65,"ID"])
length(df_spider[df_spider$ID %in% nameDT$ResponseID & df_spider[,"Communicate\nApprehension"] > .65,
                 "ID"])/nrow(df_spider)*100

rm(case_spider, df_spider, k, filenames, comm, factornames, nameDT)

#Save final workspace image
save.image()
