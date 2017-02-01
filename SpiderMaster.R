##### Spider Plot Master File #####

#Import Libraries
library(ggplot2)
library(ggradar)
suppressPackageStartupMessages(library(dplyr))
library(scales)

#Load Some Data
load('../LossAversion/cleanedData_v3.RData')

df_spider <-as.data.frame(cbind(df[,"ResponseID"],pnorm(scale(df[,c(8:12)]))))
names(df_spider) <- c("group","Lack of Preparation", "Classmates\'\nJudgment", 
                      "Being Corrected", "Being Called On", 
                      "Not Understanding\nInstructions")

#Remove
rm(df,dt,rtdf)

for (i in c(12)){ #12,18
  
  #Case Number
  print(i)
  case_spider <- df_spider[i,]
  
  #Strengths and Weaknesses
  factornames <- c("Lack of Preparation", "Classmates' Judgment", "Being Corrected",
                   "Being Called On", "Not Understanding Instructions")
  strengths <- c()
  weakness <- c()
  
  for (j in 2:length(case_spider)){
    if (case_spider[1,j] > .6){
      weakness <- append(weakness, factornames[j-1])
    }
    if (case_spider[1,j] < .4){
      strengths <- append(strengths, factornames[j-1])
    }
  }
  
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
    textboxtext_w <- "something that you do not struggle with"
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
  firstname = "Firstname"
  lastname = "Lastname"
  filename <- paste(lastname,firstname,"FLASReport", sep = "_")
  
  #Save workspace image for retrieval
  save.image()
  
  #Build HTMLs
  rmarkdown::render(input = 'SpiderReportMD_EN.Rmd', output_format = "html_document", 
                    output_file = paste('_output/', filename, ".html", sep = ""))
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
rm(case_spider, df_spider, k, filenames, comm, factornames)

#Save final workspace image
save.image()
