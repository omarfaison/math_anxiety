library(tidyverse)
library(BSDA)

## follows: https://www.youtube.com/watch?v=cUe9C2RngRs
data<-read.csv("ma.impulsivity.data.20200122.csv")

hi_count<-sum(data$amascat=="hi")
lo_count<-sum(data$amascat=="lo")

hmtxemt_hi<-lm(hardmathtime ~ easymathtime, filter(data, amascat=="hi"))
hmtxemt_lo<-lm(hardmathtime ~ easymathtime, filter(data, amascat=="lo"))

coef_he_hi<-summary(hmtxemt_hi)$coefficients
coef_he_lo<-summary(hmtxemt_lo)$coefficients

slope_est_he_hi<-coef_he_hi["easymathtime","Estimate"]
slope_se_he_hi<-coef_he_hi["easymathtime","Std. Error"]
slope_est_he_lo<-coef_he_lo["easymathtime","Estimate"]
slope_se_he_lo<-coef_he_lo["easymathtime","Std. Error"]sd

slope_sd_he_hi<-slope_est_he_hi*sqrt(hi_count)
slope_sd_he_lo<-slope_est_he_lo*sqrt(lo_count)

tsum.test(mean.x=slope_est_he_hi,s.x=slope_sd_he_hi, n.x=hi_count,
          mean.y=slope_est_he_lo,s.y=slope_sd_he_lo, n.y=hi_count)
