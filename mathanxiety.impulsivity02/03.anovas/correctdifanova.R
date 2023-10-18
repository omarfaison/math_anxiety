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
correctdiflong<-longdata %>%
  filter((str_detect(measure, pattern="dif"))) %>%
  filter(str_detect(measure, pattern="correct")) %>%
  mutate(
    difficulty=as.factor(ifelse(grepl("easy", measure), "easy",ifelse(grepl("hard", measure),"hard",NA))),
    subject=as.factor(ifelse(grepl("math", measure), "math",ifelse(grepl("verbal",measure),"verbal",NA)))
  )

subjectcorrectdiflong<-filter(correctdiflong, !is.na(subject))
difficultycorrectdiflong<-filter(correctdiflong, !is.na(difficulty))

###Subject
##run anova
subjectcorrectdifmodel<-lm(value~amascat*subject, data=subjectcorrectdiflong)
subjectcorrectdifanova<-aov(subjectcorrectdifmodel)
write.csv(tidy(subjectcorrectdifanova), "subjectcorrectdifanova.csv", row.names=F)

##post hoc pairwise t tests
#single variables
write.csv(tidy(pairwise.t.test(subjectcorrectdiflong$value,subjectcorrectdiflong$amascat,p.adj="bonf")),
          "subjectcorrectdifttestsbyamascat.csv", row.names=F)
write.csv(tidy(pairwise.t.test(subjectcorrectdiflong$value,subjectcorrectdiflong$subject,p.adj="bonf")),
          "subjectcorrectdifttestsbysubject.csv", row.names=F)
subjectcorrectdifsumstatsbyamascat<-subjectcorrectdiflong %>%
  group_by(amascat) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

subjectcorrectdifsumstatsbysubject<-subjectcorrectdiflong %>%
  group_by(subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
write.csv(subjectcorrectdifsumstatsbyamascat,"subjectcorrectdifsumstatsbyamascat.csv", row.names=F)

write.csv(subjectcorrectdifsumstatsbysubject,"subjectcorrectdifsumstatsbysubject.csv", row.names=F)

subjectcorrectdifsumstatsbyamascat.plot<-ggplot(subjectcorrectdifsumstatsbyamascat,aes(x=amascat, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="subjectcorrectdif")+
  theme_bw()

subjectcorrectdifsumstatsbysubject.plot<-ggplot(subjectcorrectdifsumstatsbysubject,aes(x=subject, y=mean, color=subject, fill=subject))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="subjectcorrectdif")+
  theme_bw()
#paired variables
write.csv(tidy(pairwise.t.test(subjectcorrectdiflong$value,subjectcorrectdiflong$amascat:subjectcorrectdiflong$subject,p.adj="bonf")),
          "subjectcorrectdifttestsbyamascatxsubject.csv", row.names=F)

subjectcorrectdifsumstatsbyamascatxsubject<-subjectcorrectdiflong %>%
  group_by(amascat,subject) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

write.csv(subjectcorrectdifsumstatsbyamascatxsubject,"subjectcorrectdifsumstatsbyamascatxsubject.csv", row.names=F)

subjectcorrectdifsumstatsbyamascatxsubject.plot<-ggplot(subjectcorrectdifsumstatsbyamascatxsubject,aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="subjectcorrectdif")+
  theme_bw()

pdf("subjectcorrectdifmeanplots.pdf")
subjectcorrectdifsumstatsbyamascat.plot
subjectcorrectdifsumstatsbysubject.plot
subjectcorrectdifsumstatsbyamascatxsubject.plot

dev.off()

###difficulty
##run anova
difficultycorrectdifmodel<-lm(value~amascat*difficulty, data=difficultycorrectdiflong)
difficultycorrectdifanova<-aov(difficultycorrectdifmodel)
write.csv(tidy(difficultycorrectdifanova), "difficultycorrectdifanova.csv", row.names=F)

##post hoc pairwise t tests
#single variables
write.csv(tidy(pairwise.t.test(difficultycorrectdiflong$value,difficultycorrectdiflong$amascat,p.adj="bonf")),
          "difficultycorrectdifttestsbyamascat.csv", row.names=F)
write.csv(tidy(pairwise.t.test(difficultycorrectdiflong$value,difficultycorrectdiflong$difficulty,p.adj="bonf")),
          "difficultycorrectdifttestsbydifficulty.csv", row.names=F)
difficultycorrectdifsumstatsbyamascat<-difficultycorrectdiflong %>%
  group_by(amascat) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

difficultycorrectdifsumstatsbydifficulty<-difficultycorrectdiflong %>%
  group_by(difficulty) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )
write.csv(difficultycorrectdifsumstatsbyamascat,"difficultycorrectdifsumstatsbyamascat.csv", row.names=F)

write.csv(difficultycorrectdifsumstatsbydifficulty,"difficultycorrectdifsumstatsbydifficulty.csv", row.names=F)

difficultycorrectdifsumstatsbyamascat.plot<-ggplot(difficultycorrectdifsumstatsbyamascat,aes(x=amascat, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="difficultycorrectdif")+
  theme_bw()

difficultycorrectdifsumstatsbydifficulty.plot<-ggplot(difficultycorrectdifsumstatsbydifficulty,aes(x=difficulty, y=mean, color=difficulty, fill=difficulty))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="difficultycorrectdif")+
  theme_bw()
#paired variables
write.csv(tidy(pairwise.t.test(difficultycorrectdiflong$value,difficultycorrectdiflong$amascat:difficultycorrectdiflong$difficulty,p.adj="bonf")),
          "difficultycorrectdifttestsbyamascatxdifficulty.csv", row.names=F)

difficultycorrectdifsumstatsbyamascatxdifficulty<-difficultycorrectdiflong %>%
  group_by(amascat,difficulty) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

write.csv(difficultycorrectdifsumstatsbyamascatxdifficulty,"difficultycorrectdifsumstatsbyamascatxdifficulty.csv", row.names=F)

difficultycorrectdifsumstatsbyamascatxdifficulty.plot<-ggplot(difficultycorrectdifsumstatsbyamascatxdifficulty,aes(x=difficulty, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(y="difficultycorrectdif")+
  theme_bw()

pdf("difficultycorrectdifmeanplots.pdf")
difficultycorrectdifsumstatsbyamascat.plot
difficultycorrectdifsumstatsbydifficulty.plot
difficultycorrectdifsumstatsbyamascatxdifficulty.plot

dev.off()
