setwd("C:/Users/midlo/Dropbox/RESEARCH/manuscripts/mathanxiety.impulsivity01/data/03.descriptives")
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(PerformanceAnalytics)

data<-read.csv("splitdata.20190903.csv", header=T)
longdata<-read.csv("splitdatalong.20190903.csv", header=T)

allmeans<-longdata %>% group_by(stat) %>%
  summarize(ave=mean(value), st_dev=sd(value), se=sd(value)/sqrt(length(value)), N=length(!is.na(value))) 
write.csv(allmeans, "alldescriptives.20190904.csv", row.names=F)

mameans<-longdata %>% group_by(stat, amascat) %>%
  summarize(ave=mean(value), st_dev=sd(value), se=sd(value)/sqrt(length(value)), N=length(!is.na(value))) 
write.csv(mameans, "madescriptives.20190904.csv", row.names=F)

amaslongmeans<-mameans %>% filter(str_detect(stat, pattern="amas"))
amasmaprofiles<-ggplot(amaslongmeans,aes(x=stat, y=ave, color=amascat))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ave-se, ymax=ave+se), size=1, width=0)+ 
  geom_line(aes(group=amascat), size=1.2)+
  theme_bw()

pdf("amascatvtime.pdf")
ggplot(data, aes(x=easymathtime, y=hardmathtime, color=amascat))+
  geom_point()+
  geom_smooth(method="lm", se=F, fullrange=T)+
  theme_bw()

ggplot(data, aes(x=easymathtime, y=mathtimedif, color=amascat))+
  geom_point()+
  geom_smooth(method="lm", se=F, fullrange=T)+
  theme_bw()

ggplot(data, aes(x=hardmathtime, y=mathtimedif, color=amascat))+
  geom_point()+
  geom_smooth(method="lm", se=F, fullrange=T)+
  theme_bw()
dev.off()



impulselongmeans<-mameans %>% filter(str_detect(stat, pattern="bis")|str_detect(stat, pattern="tuck"))
impulsemaprofiles<-ggplot(impulselongmeans,aes(x=stat, y=ave, color=amascat))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ave-se, ymax=ave+se), size=1, width=0)+ 
  geom_line(aes(group=amascat), size=1.2)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  theme_bw()

timelongmeans<-mameans %>% filter(str_detect(stat, pattern="time")) %>%
filter(str_detect(stat, pattern="easy")|str_detect(stat, pattern="hard"))
timemaprofiles<-ggplot(timelongmeans,aes(x=stat, y=ave, color=amascat))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ave-se, ymax=ave+se), size=1, width=0)+ 
  geom_line(aes(group=amascat), size=1.2)+
  theme_bw()

correctlongmeans<-mameans %>% filter(str_detect(stat, pattern="correct")) %>%
  filter(str_detect(stat, pattern="easy")|str_detect(stat, pattern="hard"))
correctmaprofiles<-ggplot(correctlongmeans,aes(x=stat, y=ave, color=amascat))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ave-se, ymax=ave+se), size=1, width=0)+ 
  geom_line(aes(group=amascat), size=1.2)+
  theme_bw()

diflongmeans<-mameans %>% filter(str_detect(stat, pattern="dif")) 
difmaprofiles<-ggplot(diflongmeans,aes(x=stat, y=ave, color=amascat))+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=ave-se, ymax=ave+se), size=1, width=0)+ 
  geom_line(aes(group=amascat), size=1.2)+
  theme_bw()

pdf(file="maprofiles.pdf")
amasmaprofiles
impulsemaprofiles
timemaprofiles
correctmaprofiles
difmaprofiles
dev.off()

numdata<-select(data, amas_learn:verbaltimedif)
pdf(file="fullcorrelationmatrix.pdf")
chart.Correlation(numdata, histogram=T)
dev.off()

corstarsl <- function(x){ 
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

cortable<-corstarsl(numdata)
write.csv(cortable,"fullcorrelationmatrix.20190904.csv")

stats<-names(numdata)
sink("amascatanovas.20190904.txt")
for (i in stats){
  graphdata=filter(longdata, stat==i)
  print(paste(i," ANOVA",sep=""))
  graphanova<-aov(value~amascat, data=graphdata)
  print(summary(graphanova))
  print(TukeyHSD(graphanova))
  print("*** ")
}
sink()