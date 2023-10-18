setwd("C:/Users/midlo/Dropbox/RESEARCH/manuscripts/mathanxiety.impulsivity02/04.regression")
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(broom)


#read data in
data<-read.csv("ma.impulsivity.data.20200122.csv", header=T)
longdata<-read.csv("ma.impulsivity.longdata.20200122.csv", header=T)

data<-data %>%
  select(id, amascat,biscat, everything())

hiamasdata<-data %>%
  filter(amascat=="hi")

loamasdata<-data %>%
  filter(amascat=="lo")

hibisdata<-data %>%
  filter(biscat=="hi")

lobisdata<-data %>%
  filter(biscat=="lo")

cordata<-select(data,amas_learn:hardtimediff)
hiamascordata<-select(hiamasdata,amas_learn:hardtimediff)
loamascordata<-select(loamasdata,amas_learn:hardtimediff)
hibiscordata<-select(hibisdata,amas_learn:hardtimediff)
lobiscordata<-select(lobisdata,amas_learn:hardtimediff)


  require(Hmisc) 
  x <- as.matrix(x) 
  R <- rcorr(x)$r 
  p <- rcorr(x)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "***", ifelse(p < .01, "** ", ifelse(p < .05, "* ", " ")))
  
  ## trunctuate the matrix that holds the correlations to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1] 
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x)) 
  diag(Rnew) <- paste(diag(R), " ", sep="") 
  rownames(Rnew) <- colnames(x) 
  colnames(Rnew) <- paste(colnames(x), "", sep="") 
  
  ## remove upper triangle
  Rnew <- as.matrix(Rnew)
  Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
  Rnew <- as.data.frame(Rnew) 
  
  ## remove last column and return the matrix (which is now a data frame)
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  return(Rnew) 
}

write.csv(corstarsl(cordata),"correlationmatrix.alldata.csv",row.names=T)
write.csv(corstarsl(hiamascordata),"correlationmatrix.hiamasdata.csv",row.names=T)
write.csv(corstarsl(loamascordata),"correlationmatrix.loamasdata.csv",row.names=T)
write.csv(corstarsl(hibiscordata),"correlationmatrix.hibisdata.csv",row.names=T)
write.csv(corstarsl(lobiscordata),"correlationmatrix.lobisdata.csv",row.names=T)
