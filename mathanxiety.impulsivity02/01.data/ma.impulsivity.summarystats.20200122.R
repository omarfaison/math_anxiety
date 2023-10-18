setwd("C:/Users/midlo/Dropbox/RESEARCH/manuscripts/mathanxiety.impulsivity02/02.summarystats")
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)


#read data in
data<-read.csv("ma.impulsivity.data.20200122.csv", header=T)
longdata<-read.csv("ma.impulsivity.longdata.20200122.csv", header=T)

#whole group summary stats
sumstats<-longdata %>%
  group_by(measure) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

write.csv(sumstats, "summarystats.20200125.csv", row.names=F)

#summary stats by amas group
amassumstats<-longdata %>%
  group_by(measure,amascat) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

write.csv(amassumstats, "summarystatsbyamascat.20200125.csv", row.names=F)

#summary stats by bis group
bissumstats<-longdata %>%
  group_by(measure,biscat) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

write.csv(bissumstats, "summarystatsbybiscat.20200125.csv", row.names=F)

#summary stats by amasxbis group
amasxbissumstats<-longdata %>%
  group_by(measure,amascat, biscat) %>%
  summarize(
    mean=mean(value),
    median=median(value),
    stddev=sd(value),
    sterr=sd(value)/sqrt(length(value)),
    N=length(value)
  )

write.csv(amasxbissumstats, "summarystatsbyamasxbiscat.20200125.csv", row.names=F)

#graph means for amas split summary stats
pdf("amassplitmeans.pdf")
amassumstats.amas<-filter(amassumstats,(str_detect(measure, pattern="amas")))
ggplot(amassumstats.amas,aes(x=measure, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  theme_bw()

amassumstats.bis<-filter(amassumstats,(str_detect(measure, pattern="bis")))
ggplot(amassumstats.bis,aes(x=measure, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  theme_bw()

amassumstats.task<-filter(amassumstats,(str_detect(measure, pattern=("easy|hard|verbal|math"))))
amassumstats.taskscores<-filter(amassumstats.task, !(str_detect(measure, pattern="dif")))

amassumstats.taskscores.splitmeasure<-amassumstats.taskscores %>%
  mutate(
    difficulty=ifelse(grepl("easy", measure), "easy","hard"),
    subject=ifelse(grepl("math", measure), "math","verbal"),
    stat=ifelse(grepl("correct",measure),"correct","time")
  )

ggplot(filter(amassumstats.taskscores.splitmeasure, stat=="correct"),aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "Correct")+
  facet_grid(. ~ difficulty)+
  theme_bw()

ggplot(filter(amassumstats.taskscores.splitmeasure, stat=="time"),aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "Time")+
  facet_grid(. ~ difficulty)+
  theme_bw()

amassumstats.difscores<-filter(amassumstats.task, (str_detect(measure, pattern="dif")))

amassumstats.difscores.splitmeasure<-amassumstats.difscores %>%
  mutate(
    difficulty=ifelse(grepl("easy", measure), "easy",ifelse(grepl("hard", measure),"hard",NA)),
    subject=ifelse(grepl("math", measure), "math",ifelse(grepl("verbal",measure),"verbal",NA)),
    stat=ifelse(grepl("correct",measure),"correct","time")
  )

ggplot(filter(amassumstats.difscores.splitmeasure, stat=="correct", !is.na(subject)),aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "Correctdif")+
  theme_bw()

ggplot(filter(amassumstats.difscores.splitmeasure, stat=="correct", !is.na(difficulty)),aes(x=difficulty, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "Correctdif")+
  theme_bw()

ggplot(filter(amassumstats.difscores.splitmeasure, stat=="time", !is.na(subject)),aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "timedif")+
  theme_bw()

ggplot(filter(amassumstats.difscores.splitmeasure, stat=="time", !is.na(difficulty)),aes(x=difficulty, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "timedif")+
  theme_bw()
dev.off()

#graph means for bis split summary stats
pdf("bissplitmeans.pdf")
bissumstats.amas<-filter(bissumstats,(str_detect(measure, pattern="amas")))
ggplot(bissumstats.amas,aes(x=measure, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  theme_bw()

bissumstats.bis<-filter(bissumstats,(str_detect(measure, pattern="bis")))
ggplot(bissumstats.bis,aes(x=measure, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  theme_bw()

bissumstats.task<-filter(bissumstats,(str_detect(measure, pattern=("easy|hard|verbal|math"))))
bissumstats.taskscores<-filter(bissumstats.task, !(str_detect(measure, pattern="dif")))

bissumstats.taskscores.splitmeasure<-bissumstats.taskscores %>%
  mutate(
    difficulty=ifelse(grepl("easy", measure), "easy","hard"),
    subject=ifelse(grepl("math", measure), "math","verbal"),
    stat=ifelse(grepl("correct",measure),"correct","time")
  )

ggplot(filter(bissumstats.taskscores.splitmeasure, stat=="correct"),aes(x=subject, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "Correct")+
  facet_grid(. ~ difficulty)+
  theme_bw()

ggplot(filter(bissumstats.taskscores.splitmeasure, stat=="time"),aes(x=subject, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "Time")+
  facet_grid(. ~ difficulty)+
  theme_bw()

bissumstats.difscores<-filter(bissumstats.task, (str_detect(measure, pattern="dif")))

bissumstats.difscores.splitmeasure<-bissumstats.difscores %>%
  mutate(
    difficulty=ifelse(grepl("easy", measure), "easy",ifelse(grepl("hard", measure),"hard",NA)),
    subject=ifelse(grepl("math", measure), "math",ifelse(grepl("verbal",measure),"verbal",NA)),
    stat=ifelse(grepl("correct",measure),"correct","time")
  )

ggplot(filter(bissumstats.difscores.splitmeasure, stat=="correct", !is.na(subject)),aes(x=subject, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "Correctdif")+
  theme_bw()

ggplot(filter(bissumstats.difscores.splitmeasure, stat=="correct", !is.na(difficulty)),aes(x=difficulty, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "Correctdif")+
  theme_bw()

ggplot(filter(bissumstats.difscores.splitmeasure, stat=="time", !is.na(subject)),aes(x=subject, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "timedif")+
  theme_bw()

ggplot(filter(bissumstats.difscores.splitmeasure, stat=="time", !is.na(difficulty)),aes(x=difficulty, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "timedif")+
  theme_bw()
dev.off()

pdf("bisxamassplitmeans.pdf")
amasxbissumstats.amas<-filter(amasxbissumstats,(str_detect(measure, pattern="amas")))
ggplot(amasxbissumstats.amas,aes(x=measure, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  facet_grid(.~amascat)+
  labs(title="BISxAMAS: AMAS")+
  theme_bw()

amasxbissumstats.bis<-filter(amasxbissumstats,(str_detect(measure, pattern="bis")))
ggplot(amasxbissumstats.bis,aes(x=measure, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  facet_grid(.~amascat)+
  labs(title="BISxAMAS: BIS")+
  theme_bw()

amasxbissumstats.task<-filter(amasxbissumstats,(str_detect(measure, pattern=("easy|hard|verbal|math"))))
amasxbissumstats.taskscores<-filter(amasxbissumstats.task, !(str_detect(measure, pattern="dif")))

amasxbissumstats.taskscores.splitmeasure<-amasxbissumstats.taskscores %>%
  mutate(
    difficulty=ifelse(grepl("easy", measure), "easy","hard"),
    subject=ifelse(grepl("math", measure), "math","verbal"),
    stat=ifelse(grepl("correct",measure),"correct","time")
  )

ggplot(filter(amasxbissumstats.taskscores.splitmeasure, stat=="correct"),aes(x=subject, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "BISxAMAS: Correct")+
  facet_grid(amascat ~ difficulty)+
  theme_bw()

ggplot(filter(amasxbissumstats.taskscores.splitmeasure, stat=="time"),aes(x=subject, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "BISxAMAS: Time")+
  facet_grid(amascat ~ difficulty)+
  theme_bw()

amasxbissumstats.difscores<-filter(amasxbissumstats.task, (str_detect(measure, pattern="dif")))

amasxbissumstats.difscores.splitmeasure<-amasxbissumstats.difscores %>%
  mutate(
    difficulty=ifelse(grepl("easy", measure), "easy",ifelse(grepl("hard", measure),"hard",NA)),
    subject=ifelse(grepl("math", measure), "math",ifelse(grepl("verbal",measure),"verbal",NA)),
    stat=ifelse(grepl("correct",measure),"correct","time")
  )

ggplot(filter(amasxbissumstats.difscores.splitmeasure, stat=="correct", !is.na(subject)),aes(x=subject, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "BISxAMAS: Correctdif")+
  facet_grid(.~amascat)+
  theme_bw()

ggplot(filter(amasxbissumstats.difscores.splitmeasure, stat=="correct", !is.na(difficulty)),aes(x=difficulty, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "BISxAMAS: Correctdif")+
  facet_grid(.~amascat)+
  theme_bw()

ggplot(filter(amasxbissumstats.difscores.splitmeasure, stat=="time", !is.na(subject)),aes(x=subject, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "BISxAMAS: timedif")+
  facet_grid(.~amascat)+
  theme_bw()

ggplot(filter(amasxbissumstats.difscores.splitmeasure, stat=="time", !is.na(difficulty)),aes(x=difficulty, y=mean, color=biscat, fill=biscat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "BISxAMAS: timedif")+
  facet_grid(.~amascat)+
  theme_bw()
dev.off()

pdf("amasxbissplitmeans.pdf")
amasxbissumstats.amas<-filter(amasxbissumstats,(str_detect(measure, pattern="amas")))
ggplot(amasxbissumstats.amas,aes(x=measure, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  facet_grid(.~biscat)+
  labs(title="AMASxBIS: AMAS")+
  theme_bw()

amasxbissumstats.bis<-filter(amasxbissumstats,(str_detect(measure, pattern="bis")))
ggplot(amasxbissumstats.bis,aes(x=measure, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  facet_grid(.~biscat)+
  labs(title="AMASxBIS: BIS")+
  theme_bw()

amasxbissumstats.task<-filter(amasxbissumstats,(str_detect(measure, pattern=("easy|hard|verbal|math"))))
amasxbissumstats.taskscores<-filter(amasxbissumstats.task, !(str_detect(measure, pattern="dif")))

amasxbissumstats.taskscores.splitmeasure<-amasxbissumstats.taskscores %>%
  mutate(
    difficulty=ifelse(grepl("easy", measure), "easy","hard"),
    subject=ifelse(grepl("math", measure), "math","verbal"),
    stat=ifelse(grepl("correct",measure),"correct","time")
  )

ggplot(filter(amasxbissumstats.taskscores.splitmeasure, stat=="correct"),aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "AMASxBIS: Correct")+
  facet_grid(biscat ~ difficulty)+
  theme_bw()

ggplot(filter(amasxbissumstats.taskscores.splitmeasure, stat=="time"),aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "AMASxBIS: Time")+
  facet_grid(biscat ~ difficulty)+
  theme_bw()

amasxbissumstats.difscores<-filter(amasxbissumstats.task, (str_detect(measure, pattern="dif")))

amasxbissumstats.difscores.splitmeasure<-amasxbissumstats.difscores %>%
  mutate(
    difficulty=ifelse(grepl("easy", measure), "easy",ifelse(grepl("hard", measure),"hard",NA)),
    subject=ifelse(grepl("math", measure), "math",ifelse(grepl("verbal",measure),"verbal",NA)),
    stat=ifelse(grepl("correct",measure),"correct","time")
  )

ggplot(filter(amasxbissumstats.difscores.splitmeasure, stat=="correct", !is.na(subject)),aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "AMASxBIS: Correctdif")+
  facet_grid(.~biscat)+
  theme_bw()

ggplot(filter(amasxbissumstats.difscores.splitmeasure, stat=="correct", !is.na(difficulty)),aes(x=difficulty, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "AMASxBIS: Correctdif")+
  facet_grid(.~biscat)+
  theme_bw()

ggplot(filter(amasxbissumstats.difscores.splitmeasure, stat=="time", !is.na(subject)),aes(x=subject, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "AMASxBIS: timedif")+
  facet_grid(.~biscat)+
  theme_bw()

ggplot(filter(amasxbissumstats.difscores.splitmeasure, stat=="time", !is.na(difficulty)),aes(x=difficulty, y=mean, color=amascat, fill=amascat))+
  geom_bar(position=position_dodge(), stat="identity")+
  geom_errorbar(aes(ymin=mean-sterr, ymax=mean+sterr), size=1, width=0, position=position_dodge(0.9))+
  geom_text(aes(label = round(mean, digits=2)), hjust = 0, vjust = 0,  position=position_dodge(0.9))+
  labs(title = "AMASxBIS: timedif")+
  facet_grid(.~biscat)+
  theme_bw()
dev.off()