setwd("C:/Users/midlo/Dropbox/RESEARCH/manuscripts/mathanxiety.impulsivity02/03.anovas")
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(broom)


#read data in
data<-read.csv("ma.impulsivity.data.20200122.csv", header=T)
longdata<-read.csv("ma.impulsivity.longdata.20200122.csv", header=T)

##anova on time data
##prep data
timelong<-longdata %>%
  filter(!(str_detect(measure, pattern="dif"))) %>%
  filter(str_detect(measure, pattern="time")) %>%
  mutate(
    difficulty=as.factor(ifelse(grepl("easy", measure), "easy",ifelse(grepl("hard", measure),"hard",NA))),
    subject=as.factor(ifelse(grepl("math", measure), "math",ifelse(grepl("verbal",measure),"verbal",NA)))
  )

##run anova
timemodel<-lm(value~amascat*difficulty*subject, data=timelong)
timeanova<-aov(timemodel)
write.csv(tidy(timeanova), "timeanova.csv", row.names=F)

##post hoc pairwise t tests
#single variables
write.csv(tidy(pairwise.t.test(timelong$value,timelong$amascat,p.adj="bonf")),
          "timettestsbyamascat.csv", row.names=F)
write.csv(tidy(pairwise.t.test(timelong$value,timelong$difficulty,p.adj="bonf")),
          "timettestsbydifficulty.csv", row.names=F)
write.csv(tidy(pairwise.t.test(timelong$value,timelong$subject,p.adj="bonf")),
          "timettestsbysubject", row.names=F)
timesumstatsbyamascat<-timelong %>%
  group_by(amascat) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
timesumstatsbydifficulty<-timelong %>%
  group_by(difficulty) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
timesumstatsbysubject<-timelong %>%
  group_by(subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
write.csv(timesumstatsbyamascat,"timesumstatsbyamascat.csv", row.names=F)
write.csv(timesumstatsbydifficulty,"timesumstatsbydifficulty.csv", row.names=F)
write.csv(timesumstatsbysubject,"timesumstatsbysubject.csv", row.names=F)

timesumstatsbyamascat.plot<-ggplot(timesumstatsbyamascat,aes(x=amascat, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="time")+
  theme_bw()
timesumstatsbydifficulty.plot<-ggplot(timesumstatsbydifficulty,aes(x=difficulty, y=mean, color=difficulty, fill=difficulty))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="time")+
  theme_bw()
timesumstatsbysubject.plot<-ggplot(timesumstatsbysubject,aes(x=subject, y=mean, color=subject, fill=subject))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="time")+
  theme_bw()
#paired variables
write.csv(tidy(pairwise.t.test(timelong$value,timelong$amascat:timelong$difficulty,p.adj="bonf")),
          "timettestsbyamascatxdifficulty.csv", row.names=F)
write.csv(tidy(pairwise.t.test(timelong$value,timelong$amascat:timelong$subject,p.adj="bonf")),
          "timettestsbyamascatxsubject.csv", row.names=F)
write.csv(tidy(pairwise.t.test(timelong$value,timelong$difficulty:timelong$subject,p.adj="bonf")),
          "timettestsbydifficultyxsubject.csv", row.names=F)
timesumstatsbyamascatxdifficulty<-timelong %>%
  group_by(amascat,difficulty) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
)

timesumstatsbyamascatxsubject<-timelong %>%
  group_by(amascat,subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
timesumstatsbydifficultyxsubject<-timelong %>%
  group_by(difficulty,subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
write.csv(timesumstatsbyamascatxdifficulty,"timesumstatsbyamascatxdifficulty.csv", row.names=F)
write.csv(timesumstatsbyamascatxsubject,"timesumstatsbyamascatxsubject.csv", row.names=F)
write.csv(timesumstatsbydifficultyxsubject,"timesumstatsbydifficultyxsubject.csv", row.names=F)

timesumstatsbyamascatxdifficulty.plot<-ggplot(timesumstatsbyamascatxdifficulty,aes(x=difficulty, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="time")+
  theme_bw()
timesumstatsbyamascatxsubject.plot<-ggplot(timesumstatsbyamascatxsubject,aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="time")+
  theme_bw()
timesumstatsbydifficultyxsubject.plot<-ggplot(timesumstatsbydifficultyxsubject,aes(x=subject, y=mean, color=difficulty, fill=difficulty))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="time")+
  theme_bw()

#all variables
write.csv(tidy(pairwise.t.test(timelong$value,timelong$amascat:timelong$difficulty:timelong$subject,p.adj="bonf")),
          "timettestsbyall.csv", row.names=F)
timesumstatsbyall<-timelong %>%
  group_by(amascat,difficulty,subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
write.csv(timesumstatsbyall,"timesumstatsbyall.csv", row.names=F)

timesumstatsbyall.subjectx.plot<-ggplot(timesumstatsbyall,aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  facet_grid(.~difficulty)+
  labs(y="time")+
  theme_bw()

timesumstatsbyall.difficultyx.plot<-ggplot(timesumstatsbyall,aes(x=difficulty, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  facet_grid(.~subject)+
  labs(y="time")+
  theme_bw()

pdf("timemeanplots.pdf")
timesumstatsbyamascat.plot
timesumstatsbydifficulty.plot
timesumstatsbysubject.plot
timesumstatsbyamascatxdifficulty.plot
timesumstatsbyamascatxsubject.plot
timesumstatsbydifficultyxsubject.plot
timesumstatsbyall.subjectx.plot
timesumstatsbyall.difficultyx.plot
dev.off()
