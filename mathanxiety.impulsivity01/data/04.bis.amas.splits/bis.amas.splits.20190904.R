setwd("C:/Users/midlo/Dropbox/RESEARCH/manuscripts/mathanxiety.impulsivity01/data/04.bis.amas.splits")
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)

data<-read.csv("splitdata.20190903.csv", header=T)

pdf("bisvtimebyamas.pdf")
ggplot(data, aes(x=bis, y=easymathtime, color=amascat))+
  geom_point()+
  geom_smooth(method="lm", se=F, fullrange=T)+
  theme_bw()

ggplot(data, aes(x=bis, y=hardmathtime, color=amascat))+
  geom_point()+
  geom_smooth(method="lm", se=F, fullrange=T)+
  theme_bw()

ggplot(data, aes(x=bis, y=mathtimedif, color=amascat))+
  geom_point()+
  geom_smooth(method="lm", se=F, fullrange=T)+
  theme_bw()
dev.off()

bissplit<-data %>%
  mutate(biscat=ifelse(bis>median(bis, na.rm=T),"hi", ifelse(bis<=median(bis, na.rm=T),"lo"," ")))

write.csv(bissplit,"bis.amas.split.csv", row.names=F)