# #### Neural Network Plot Resources ####
# # https://briatte.github.io/ggnet/
# # https://cran.r-project.org/web/packages/ggnetwork/vignettes/ggnetwork.html
# 
#Import Simplified Takeshi
# tak <- read.csv('_data/simplified_takeshi.csv')
# tak$description<-sapply(tak$description, tolower)
# 
# x<-table(tak$description)
# x<-rownames(x[x>2])
# x<-x[-c(15,16)]
# 
# takJ<-subset(tak, description %in% x & group == "JN",
#              select = c("group","participant","order","description"))
# takE<-subset(tak, description %in% x & group == "EA",
#              select = c("group","participant","order","description"))
# 
# takJ <- takJ[takJ$participant != "4",]
# takJ <- takJ[takJ$participant != "9",]
# takJ <- takJ[takJ$participant != "16",]
# takJ <- takJ[takJ$participant != "18",]
# takJ <- takJ[takJ$participant != "25",]
# takE <- takE[takE$participant != "27",]
# takE <- takE[takE$participant != "36",]
# takE <- takE[takE$participant != "38",]
# takE <- takE[takE$participant != "42",]
# takE <- takE[takE$participant != "43",]
# takE <- takE[takE$participant != "45",]
# 
# table(takJ$participant)
# table(takE$participant)

load('takeshi.RData')

#Node Size
# Number of endorsements by average order
NodeWeight <- function(dt){
  require(doBy)
  scalar<-summaryBy(order ~ participant, dt, FUN = max)
  
  for (j in 1:nrow(scalar)){
    for (i in 1:nrow(dt)){
      if (dt[i, "participant"] == scalar[j, "participant"]){
        dt[i,"order"] <- (scalar[j,"order.max"]+1) - dt[i,"order"]
      }
    }
  }
  
  ave_endorse <- table(dt$description)
  ave_order <- summaryBy(order ~ dt$description, dt)
  ave_order$endorse <- ave_endorse
  ave_order$node_weight <- 1+pnorm(scale(ave_order$endorse * ave_order$order.mean))
  return(ave_order$node_weight)
}

#Node Distance
NodeDistance <- function(dt){
  scalar<-summaryBy(order ~ participant, dt, FUN = max)
  des <- sort(unique(dt$description))
  distMat <- list()
  
  for (j in 1:nrow(scalar)){
    x <- dt[dt$participant == scalar[j, "participant"],]
    
    mat <- matrix(0,nrow(x),nrow(x))
    rownames(mat) <- x$description
    colnames(mat) <- x$description
    mat[upper.tri(mat)] <- rev(abs(apply(combn(x$order,2), 2, diff)))
    mat[lower.tri(mat)] <- abs(apply(combn(x$order,2), 2, diff)) #Matrix not right
    
    distMat[[j]] <- mat
  }
  
  finalmatrix <- matrix(0, length(des), length(des))
  rownames(finalmatrix) <- des
  colnames(finalmatrix) <- des
  
  for (i in 1:length(distMat)){
    finalmatrix[rownames(distMat[[i]]), colnames(distMat[[i]])] <- (distMat[[i]] +
    finalmatrix[rownames(distMat[[i]]), colnames(distMat[[i]])])/2
  }
  
  library(car)
  finalmatrix <- recode(finalmatrix, "0 = NA")
  diag(finalmatrix) <- 0
  
  return(finalmatrix + 1)
}

#### Neural Network ####
library(ggnet)
library(network)
library(sna)
library(ggplot2)
library(grid)
library(gridExtra)

for (i in 1:30){

  takE_data<-data.frame(descriptions = rownames(NodeWeight(takE)), node_weights = as.vector(NodeWeight(takE)))
  takJ_data<-data.frame(descriptions = rownames(NodeWeight(takJ)), node_weights = as.vector(NodeWeight(takJ)))
  
  takeE_struct<-data.frame(NodeDistance(takE))
  takeJ_struct<-data.frame(NodeDistance(takJ))
  
  Enet = network(apply(takeE_struct, 2, function(x) {recode(x, "NA = 0; else = 1")}), directed = FALSE)
  network.vertex.names(Enet) = letters[1:10]
  
  Eplot<-ggnet2(Enet, size = takE_data$node_weights*5, node.label = rownames(takeE_struct), label.size = 4,
                node.alpha = .8, edge.size = na.omit(takeE_struct[lower.tri(takeE_struct)])/2,
                edge.alpha = .6) + theme(legend.position = "none")
  
  Jnet = network(apply(takeJ_struct, 2, function(x) {recode(x, "NA = 0; else = 1")}), directed = FALSE)
  network.vertex.names(Jnet) = letters[1:10]
  
  Jplot<-ggnet2(Jnet, size = takJ_data$node_weights*5, node.label = rownames(takeJ_struct), label.size = 4,
                node.alpha = .8, edge.size = na.omit(takeJ_struct[lower.tri(takeJ_struct)])/2,
                edge.alpha = .6) + theme(legend.position = "none")
  
  lay <- rbind(c(1,1,2,2),
               c(1,1,2,2)) #4 in place of NA
  
  ##Save as image and load into rmd
  png(filename=paste("_images/plot",i,".png"), width = 1250, height = 500, units = "px")
  grid.arrange(Jplot, Eplot, layout_matrix = lay)
  dev.off()
}

#8, 18, 20
