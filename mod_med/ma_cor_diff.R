library(psych)

data<-read.csv("ma.impulsivity.data.20200122.csv")
hi_count<-sum(data$amascat=="hi")
lo_count<-sum(data$amascat=="lo")

hma_data<-filter(data, amascat=="hi")
lma_data<-filter(data, amascat=="lo")

cor_hmtxemt_hi<-cor(hma_data$easymathtime, hma_data$hardmathtime)
cor_hmtxemt_lo<-cor(lma_data$easymathtime, lma_data$hardmathtime)

paired.r(cor_hmtxemt_hi, cor_hmtxemt_lo, NULL, hi_count, lo_count)

