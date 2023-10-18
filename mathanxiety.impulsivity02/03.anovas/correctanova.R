setwd("C:/Users/midlo/Dropbox/RESEARCH/manuscripts/mathanxiety.impulsivity02/03.anovas")
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(broom)


#read data in
data<-read.csv("ma.impulsivity.data.20200122.csv", header=T)
longdata<-read.csv("ma.impulsivity.longdata.20200122.csv", header=T)

##anova on correct data
##prep data
correctlong<-longdata %>%
  filter(!(str_detect(measure, pattern="dif"))) %>%
  filter(str_detect(measure, pattern="correct")) %>%
  mutate(
    difficulty=as.factor(ifelse(grepl("easy", measure), "easy",ifelse(grepl("hard", measure),"hard",NA))),
    subject=as.factor(ifelse(grepl("math", measure), "math",ifelse(grepl("verbal",measure),"verbal",NA)))
  )

##run anova
correctmodel<-lm(value~amascat*difficulty*subject, data=correctlong)
correctanova<-aov(correctmodel)
write.csv(tidy(correctanova), "correctanova.csv", row.names=F)

##post hoc pairwise t tests
#single variables
write.csv(tidy(pairwise.t.test(correctlong$value,correctlong$amascat,p.adj="bonf")),
          "correctttestsbyamascat.csv", row.names=F)
write.csv(tidy(pairwise.t.test(correctlong$value,correctlong$difficulty,p.adj="bonf")),
          "correctttestsbydifficulty.csv", row.names=F)
write.csv(tidy(pairwise.t.test(correctlong$value,correctlong$subject,p.adj="bonf")),
          "correctttestsbysubject", row.names=F)
correctsumstatsbyamascat<-correctlong %>%
  group_by(amascat) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
correctsumstatsbydifficulty<-correctlong %>%
  group_by(difficulty) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
correctsumstatsbysubject<-correctlong %>%
  group_by(subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
write.csv(correctsumstatsbyamascat,"correctsumstatsbyamascat.csv", row.names=F)
write.csv(correctsumstatsbydifficulty,"correctsumstatsbydifficulty.csv", row.names=F)
write.csv(correctsumstatsbysubject,"correctsumstatsbysubject.csv", row.names=F)

correctsumstatsbyamascat.plot<-ggplot(correctsumstatsbyamascat,aes(x=amascat, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="correct")+
  theme_bw()
correctsumstatsbydifficulty.plot<-ggplot(correctsumstatsbydifficulty,aes(x=difficulty, y=mean, color=difficulty, fill=difficulty))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="correct")+
  theme_bw()
correctsumstatsbysubject.plot<-ggplot(correctsumstatsbysubject,aes(x=subject, y=mean, color=subject, fill=subject))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="correct")+
  theme_bw()
#paired variables
write.csv(tidy(pairwise.t.test(correctlong$value,correctlong$amascat:correctlong$difficulty,p.adj="bonf")),
          "correctttestsbyamascatxdifficulty.csv", row.names=F)
write.csv(tidy(pairwise.t.test(correctlong$value,correctlong$amascat:correctlong$subject,p.adj="bonf")),
          "correctttestsbyamascatxsubject.csv", row.names=F)
write.csv(tidy(pairwise.t.test(correctlong$value,correctlong$difficulty:correctlong$subject,p.adj="bonf")),
          "correctttestsbydifficultyxsubject.csv", row.names=F)
correctsumstatsbyamascatxdifficulty<-correctlong %>%
  group_by(amascat,difficulty) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
)

correctsumstatsbyamascatxsubject<-correctlong %>%
  group_by(amascat,subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
correctsumstatsbydifficultyxsubject<-correctlong %>%
  group_by(difficulty,subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
write.csv(correctsumstatsbyamascatxdifficulty,"correctsumstatsbyamascatxdifficulty.csv", row.names=F)
write.csv(correctsumstatsbyamascatxsubject,"correctsumstatsbyamascatxsubject.csv", row.names=F)
write.csv(correctsumstatsbydifficultyxsubject,"correctsumstatsbydifficultyxsubject.csv", row.names=F)

correctsumstatsbyamascatxdifficulty.plot<-ggplot(correctsumstatsbyamascatxdifficulty,aes(x=difficulty, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="correct")+
  theme_bw()
correctsumstatsbyamascatxsubject.plot<-ggplot(correctsumstatsbyamascatxsubject,aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="correct")+
  theme_bw()
correctsumstatsbydifficultyxsubject.plot<-ggplot(correctsumstatsbydifficultyxsubject,aes(x=subject, y=mean, color=difficulty, fill=difficulty))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="correct")+
  theme_bw()

#all variables
write.csv(tidy(pairwise.t.test(correctlong$value,correctlong$amascat:correctlong$difficulty:correctlong$subject,p.adj="bonf")),
          "correctttestsbyall.csv", row.names=F)
correctsumstatsbyall<-correctlong %>%
  group_by(amascat,difficulty,subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
write.csv(correctsumstatsbyall,"correctsumstatsbyall.csv", row.names=F)

correctsumstatsbyall.subjectx.plot<-ggplot(correctsumstatsbyall,aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  facet_grid(.~difficulty)+
  labs(y="correct")+
  theme_bw()

correctsumstatsbyall.difficultyx.plot<-ggplot(correctsumstatsbyall,aes(x=difficulty, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  facet_grid(.~subject)+
  labs(y="correct")+
  theme_bw()

pdf("correctmeanplots.pdf")
correctsumstatsbyamascat.plot
correctsumstatsbydifficulty.plot
correctsumstatsbysubject.plot
correctsumstatsbyamascatxdifficulty.plot
correctsumstatsbyamascatxsubject.plot
correctsumstatsbydifficultyxsubject.plot
correctsumstatsbyall.subjectx.plot
correctsumstatsbyall.difficultyx.plot
dev.off()
