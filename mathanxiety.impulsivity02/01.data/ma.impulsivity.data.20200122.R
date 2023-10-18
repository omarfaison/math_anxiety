setwd("C:/Users/midlo/Dropbox/RESEARCH/manuscripts/mathanxiety.impulsivity02/01.data")
library(dplyr)
library(tidyr)
library(ggplot2)

#1.	Data from ma.impulsivity01--> cleanandmerge
data<-read.csv("mathtaskplusscores.20190903.csv", header=T)

#2.	Remove bis scales,Tuckman
data<-select(data, -(bis_attention:bis_cogcomplex), -tuckman)

#3.	Lo hi split categories
splitdata<-data %>%
  mutate(
    amascat=ifelse(amas>median(amas, na.rm=T),"hi", ifelse(amas<=median(amas, na.rm=T),"lo"," ")),
    biscat=ifelse(bis>median(bis, na.rm=T),"hi", ifelse(bis<=median(bis, na.rm=T),"lo"," "))
  )

scalemedians<-splitdata %>%
  summarize(
    amasmedian=median(amas),
    bismedian=median(bis)
  )

write.csv(scalemedians, "scalemedian.csv", row.names=F)

#4.	calculate Difference scores

data<-splitdata %>%
  mutate(
    #easycorrect-hardcorrect
    mathcorrectdif=easymathcorrect-hardmathcorrect,
    verbalcorrectdif=easyverbalcorrect-hardverbalcorrect,
    #hardtime-easytime
    mathtimediff=hardmathtime-easymathtime,
    verbaltimediff=hardverbaltime-easyverbaltime,
    #verbal-math correct
    easycorrectdiff=easyverbalcorrect-easymathcorrect,
    hardcorrectdiff=hardverbalcorrect-hardmathcorrect,
    #math-verbal time
    easytimediff=easymathtime-easyverbaltime,
    hardtimediff=hardmathtime-hardverbaltime
  )

write.csv(data, "ma.impulsivity.data.20200122.csv", row.names=F)

# create long format
longdata<-data %>%
  gather("measure", "value",c(amas_learn:hardverbalcorrect,mathcorrectdif:hardtimediff))

write.csv(longdata, "ma.impulsivity.longdata.20200122.csv", row.names=F)
  