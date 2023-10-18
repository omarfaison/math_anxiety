setwd("C:/Users/midlo/Dropbox/RESEARCH/manuscripts/mathanxiety.impulsivity01/data/02.cleanandmerge")
library(dplyr)
library(tidyr)

data<-read.csv("mathanxietysurveys.20190421.csv", header=T)

#AMAS
amas<-data %>% select(id, starts_with("amas")) %>%
  mutate(learn01=AMAS1,
         eval02=AMAS2,
         learn03=AMAS3,
         eval04=AMAS4,
         eval05=AMAS5,
         learn06=AMAS6,
         learn07=AMAS7,
         eval08=AMAS8,
         learn09=AMAS9)

amasscores<-amas %>% transmute(id,
                               amas_learn=rowMeans(select(.,starts_with('learn')),na.rm=TRUE),
                               amas_eval=rowMeans(select(.,starts_with('eval')),na.rm=TRUE),
                               amas=rowMeans(select(.,learn01:learn09),na.rm=TRUE))

write.csv(amas,"amas.csv",row.names=F)
write.csv(amasscores,"amasscores.csv",row.names=F)

#BIS-11
bis<-data %>% select(id, starts_with("BIS")) %>%
  mutate(selfcntrl01=5-BIS_1,
         motor02=BIS_2,
         motor03=BIS_3,
         motor04=BIS_4,
         attention05=BIS_5,
         coginstable06=BIS_6,
         selfcntrl07=5-BIS_7,
         selfcntrl08=5-BIS_8,
         attention09=5-BIS_9,
         cogcomplex10=5-BIS_10,
         attention11=BIS_11,
         selfcntrl12=5-BIS_12,
         selfcntrl13=5-BIS_13,
         selfcntrl14=BIS_14,
         cogcomplex15=5-BIS_15,
         persev16=BIS_16,
         motor17=BIS_17,
         cogcomplex18=BIS_18,
         motor19=BIS_19,
         attention20=5-BIS_20,
         persev21=BIS_21,
         motor22=BIS_22,
         persev23=BIS_23,
         coginstable24=BIS_24,
         motor25=BIS_25,
         coginstable26=BIS_26,
         cogcomplex27=BIS_27,
         attention28=BIS_28,
         cogcomplex29=5-BIS_29,
         persev30=5-BIS_30)

bisscores<-bis %>% transmute(id,
                             bis_attention=rowMeans(select(.,starts_with('attention')),na.rm=TRUE),
                             bis_coginstable=rowMeans(select(.,starts_with('coginstable')),na.rm=TRUE),
                             bis_motor=rowMeans(select(.,starts_with('motor')),na.rm=TRUE),
                             bis_persev=rowMeans(select(.,starts_with('persev')),na.rm=TRUE),
                             bis_selfcntrl=rowMeans(select(.,starts_with('selfcntrl')),na.rm=TRUE),
                             bis_cogcomplex=rowMeans(select(.,starts_with('cogcomplex')),na.rm=TRUE),
                             bis_ATT=rowMeans(select(.,matches('attention|coginstable')),na.rm=TRUE),
                             bis_MTR=rowMeans(select(.,matches('motor|persev')),na.rm=TRUE),
                             bis_NPL=rowMeans(select(.,matches('selfcntrl|cogcomplex')),na.rm=TRUE),
                             bis=rowMeans(select(.,selfcntrl01:persev30),na.rm=TRUE))

write.csv(bis,"bis.csv",row.names=F)
write.csv(bisscores,"bisscores.csv",row.names=F)

#Tuckman
tuckman<-data %>% select(id, starts_with("Tuckman")) %>%
  mutate(tuckman01=Tuckman1,
         tuckman02=Tuckman2,
         tuckman03=5-Tuckman3,
         tuckman04=5-Tuckman4,
         tuckman05=Tuckman5,
         tuckman06=Tuckman6,
         tuckman07=Tuckman7,
         tuckman08=5-Tuckman8,
         tuckman09=Tuckman9)

tuckmanscores<-tuckman %>% transmute(id,
                                     tuckman=rowMeans(select(.,tuckman01:tuckman09),na.rm=TRUE))

write.csv(tuckman,"tuckman.csv",row.names=F)
write.csv(tuckmanscores,"tuckmanscores.csv",row.names=F)

#merge the scales
mergedscales<-amasscores %>% 
              full_join(bisscores, by="id") %>%
              full_join(tuckmanscores, by="id")

write.csv(mergedscales, "mathanxietyscales.merged.20190903.csv", row.names=F)

mathtask<-read.csv("mathtask.20190421.csv", header=T)
data<-full_join(mergedscales, mathtask, by="id") 
data<-na.omit(data)
write.csv(data,"mathtaskplusscores.20190903.csv", row.names=F)

diffdata<-data %>%
  mutate(mathcorrectdif=easymathcorrect-hardmathcorrect,
         verbalcorrectdif=easyverbalcorrect-hardverbalcorrect,
         mathtimedif=easymathtime-hardmathtime,
         verbaltimedif=easyverbaltime-hardverbaltime)

splitdata<-diffdata %>%
  mutate(amascat=ifelse(amas>median(amas, na.rm=T),"hi", ifelse(amas<=median(amas, na.rm=T),"lo"," ")))

write.csv(splitdata,"splitdata.20190903.csv", row.names=F)

timedata<-select(splitdata, id,amascat,easymathtime:hardverbaltime)
timelong<-timedata %>% gather(class, time, easymathtime:hardverbaltime)
timeclass<- timelong %>% mutate(difficulty=ifelse(grepl("easy", class), "easy","hard"),
                                subject=ifelse(grepl("math", class), "math","verbal"))
timefinal<-timeclass %>% select(id, amascat, difficulty, subject, time)

correctdata<-select(splitdata, id,amascat, easymathcorrect:hardverbalcorrect)
correctlong<-correctdata %>% gather(class, correct, easymathcorrect:hardverbalcorrect)
correctclass<- correctlong %>% mutate(difficulty=ifelse(grepl("easy", class), "easy","hard"),
                                      subject=ifelse(grepl("math", class), "math","verbal"))
correctfinal<-correctclass %>% select(id, amascat, difficulty, subject, correct)

aovdata<-inner_join(timefinal, correctfinal, by=c("id","amascat","difficulty","subject"))
write.csv(aovdata,"anovadatalong.20190903.csv", row.names=F)

splitdatalong<-gather(splitdata, stat, value, amas_learn:verbaltimedif)
write.csv(splitdatalong,"splitdatalong.20190903.csv", row.names=F)