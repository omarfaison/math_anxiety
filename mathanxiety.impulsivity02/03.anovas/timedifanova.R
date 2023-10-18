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
timediflong<-longdata %>%
  filter((str_detect(measure, pattern="dif"))) %>%
  filter(str_detect(measure, pattern="time")) %>%
  mutate(
    difficulty=as.factor(ifelse(grepl("easy", measure), "easy",ifelse(grepl("hard", measure),"hard",NA))),
    subject=as.factor(ifelse(grepl("math", measure), "math",ifelse(grepl("verbal",measure),"verbal",NA)))
  )

subjecttimediflong<-filter(timediflong, !is.na(subject))
difficultytimediflong<-filter(timediflong, !is.na(difficulty))

###Subject
##run anova
subjecttimedifmodel<-lm(value~amascat*subject, data=subjecttimediflong)
subjecttimedifanova<-aov(subjecttimedifmodel)
write.csv(tidy(subjecttimedifanova), "subjecttimedifanova.csv", row.names=F)

##post hoc pairwise t tests
#single variables
write.csv(tidy(pairwise.t.test(subjecttimediflong$value,subjecttimediflong$amascat,p.adj="bonf")),
          "subjecttimedifttestsbyamascat.csv", row.names=F)
write.csv(tidy(pairwise.t.test(subjecttimediflong$value,subjecttimediflong$subject,p.adj="bonf")),
          "subjecttimedifttestsbysubject.csv", row.names=F)
subjecttimedifsumstatsbyamascat<-subjecttimediflong %>%
  group_by(amascat) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

subjecttimedifsumstatsbysubject<-subjecttimediflong %>%
  group_by(subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
write.csv(subjecttimedifsumstatsbyamascat,"subjecttimedifsumstatsbyamascat.csv", row.names=F)

write.csv(subjecttimedifsumstatsbysubject,"subjecttimedifsumstatsbysubject.csv", row.names=F)

subjecttimedifsumstatsbyamascat.plot<-ggplot(subjecttimedifsumstatsbyamascat,aes(x=amascat, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="subjecttimedif")+
  theme_bw()

subjecttimedifsumstatsbysubject.plot<-ggplot(subjecttimedifsumstatsbysubject,aes(x=subject, y=mean, color=subject, fill=subject))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="subjecttimedif")+
  theme_bw()
#paired variables
write.csv(tidy(pairwise.t.test(subjecttimediflong$value,subjecttimediflong$amascat:subjecttimediflong$subject,p.adj="bonf")),
          "subjecttimedifttestsbyamascatxsubject.csv", row.names=F)

subjecttimedifsumstatsbyamascatxsubject<-subjecttimediflong %>%
  group_by(amascat,subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

write.csv(subjecttimedifsumstatsbyamascatxsubject,"subjecttimedifsumstatsbyamascatxsubject.csv", row.names=F)

subjecttimedifsumstatsbyamascatxsubject.plot<-ggplot(subjecttimedifsumstatsbyamascatxsubject,aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="subjecttimedif")+
  theme_bw()

pdf("subjecttimedifmeanplots.pdf")
subjecttimedifsumstatsbyamascat.plot
subjecttimedifsumstatsbysubject.plot
subjecttimedifsumstatsbyamascatxsubject.plot

dev.off()

###difficulty
##run anova
difficultytimedifmodel<-lm(value~amascat*difficulty, data=difficultytimediflong)
difficultytimedifanova<-aov(difficultytimedifmodel)
write.csv(tidy(difficultytimedifanova), "difficultytimedifanova.csv", row.names=F)

##post hoc pairwise t tests
#single variables
write.csv(tidy(pairwise.t.test(difficultytimediflong$value,difficultytimediflong$amascat,p.adj="bonf")),
          "difficultytimedifttestsbyamascat.csv", row.names=F)
write.csv(tidy(pairwise.t.test(difficultytimediflong$value,difficultytimediflong$difficulty,p.adj="bonf")),
          "difficultytimedifttestsbydifficulty.csv", row.names=F)
difficultytimedifsumstatsbyamascat<-difficultytimediflong %>%
  group_by(amascat) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

difficultytimedifsumstatsbydifficulty<-difficultytimediflong %>%
  group_by(difficulty) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
write.csv(difficultytimedifsumstatsbyamascat,"difficultytimedifsumstatsbyamascat.csv", row.names=F)

write.csv(difficultytimedifsumstatsbydifficulty,"difficultytimedifsumstatsbydifficulty.csv", row.names=F)

difficultytimedifsumstatsbyamascat.plot<-ggplot(difficultytimedifsumstatsbyamascat,aes(x=amascat, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="difficultytimedif")+
  theme_bw()

difficultytimedifsumstatsbydifficulty.plot<-ggplot(difficultytimedifsumstatsbydifficulty,aes(x=difficulty, y=mean, color=difficulty, fill=difficulty))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="difficultytimedif")+
  theme_bw()
#paired variables
write.csv(tidy(pairwise.t.test(difficultytimediflong$value,difficultytimediflong$amascat:difficultytimediflong$difficulty,p.adj="bonf")),
          "difficultytimedifttestsbyamascatxdifficulty.csv", row.names=F)

difficultytimedifsumstatsbyamascatxdifficulty<-difficultytimediflong %>%
  group_by(amascat,difficulty) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

write.csv(difficultytimedifsumstatsbyamascatxdifficulty,"difficultytimedifsumstatsbyamascatxdifficulty.csv", row.names=F)

difficultytimedifsumstatsbyamascatxdifficulty.plot<-ggplot(difficultytimedifsumstatsbyamascatxdifficulty,aes(x=difficulty, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="difficultytimedif")+
  theme_bw()

pdf("difficultytimedifmeanplots.pdf")
difficultytimedifsumstatsbyamascat.plot
difficultytimedifsumstatsbydifficulty.plot
difficultytimedifsumstatsbyamascatxdifficulty.plot

dev.off()
