library(tidyverse)
library(BSDA)

## follows: https://www.youtube.com/watch?v=cUe9C2RngRs
data<-read.csv("ma.impulsivity.data.20200122.csv")

hi_count<-sum(data$amascat=="hi")
lo_count<-sum(data$amascat=="lo")

#hard math time x easy math time
hmtxemt_hi<-lm(hardmathtime ~ easymathtime, filter(data, amascat=="hi"))
hmtxemt_lo<-lm(hardmathtime ~ easymathtime, filter(data, amascat=="lo"))

coef_he_hi<-summary(hmtxemt_hi)$coefficients
coef_he_lo<-summary(hmtxemt_lo)$coefficients

slope_est_he_hi<-coef_he_hi["easymathtime","Estimate"]
slope_se_he_hi<-coef_he_hi["easymathtime","Std. Error"]
slope_est_he_lo<-coef_he_lo["easymathtime","Estimate"]
slope_se_he_lo<-coef_he_lo["easymathtime","Std. Error"]

slope_sd_he_hi<-slope_est_he_hi*sqrt(hi_count)
slope_sd_he_lo<-slope_est_he_lo*sqrt(lo_count)

tsum.test(mean.x=slope_est_he_hi,s.x=slope_sd_he_hi, n.x=hi_count,
          mean.y=slope_est_he_lo,s.y=slope_sd_he_lo, n.y=hi_count)

#easy math time x bis
emtxbis_hi<-lm(easymathtime ~ bis, filter(data, amascat=="hi"))
emtxbis_lo<-lm(easymathtime ~ bis, filter(data, amascat=="lo"))

coef_eb_hi<-summary(emtxbis_hi)$coefficients
coef_eb_lo<-summary(emtxbis_lo)$coefficients

slope_est_eb_hi<-coef_eb_hi["bis","Estimate"]
slope_se_eb_hi<-coef_eb_hi["bis","Std. Error"]
slope_est_eb_lo<-coef_eb_lo["bis","Estimate"]
slope_se_eb_lo<-coef_eb_lo["bis","Std. Error"]

slope_sd_eb_hi<-slope_est_eb_hi*sqrt(hi_count)
slope_sd_eb_lo<-slope_est_eb_lo*sqrt(lo_count)

tsum.test(mean.x=slope_est_eb_hi,s.x=slope_sd_eb_hi, n.x=hi_count,
          mean.y=slope_est_eb_lo,s.y=slope_sd_eb_lo, n.y=hi_count)

#hard math time x bis
hmtxbis_hi<-lm(hardmathtime ~ bis, filter(data, amascat=="hi"))
hmtxbis_lo<-lm(hardmathtime ~ bis, filter(data, amascat=="lo"))

coef_hb_hi<-summary(hmtxbis_hi)$coefficients
coef_hb_lo<-summary(hmtxbis_lo)$coefficients

slope_est_hb_hi<-coef_hb_hi["bis","Estimate"]
slope_se_hb_hi<-coef_hb_hi["bis","Std. Error"]
slope_est_hb_lo<-coef_hb_lo["bis","Estimate"]
slope_se_hb_lo<-coef_hb_lo["bis","Std. Error"]

slope_sd_hb_hi<-slope_est_hb_hi*sqrt(hi_count)
slope_sd_hb_lo<-slope_est_hb_lo*sqrt(lo_count)

tsum.test(mean.x=slope_est_hb_hi,s.x=slope_sd_hb_hi, n.x=hi_count,
          mean.y=slope_est_hb_lo,s.y=slope_sd_hb_lo, n.y=hi_count)

#math time diff x bis
mtdxbis_hi<-lm(mathtimediff ~ bis, filter(data, amascat=="hi"))
mtdxbis_lo<-lm(mathtimediff ~ bis, filter(data, amascat=="lo"))

coef_db_hi<-summary(mtdxbis_hi)$coefficients
coef_db_lo<-summary(mtdxbis_lo)$coefficients

slope_est_db_hi<-coef_db_hi["bis","Estimate"]
slope_se_db_hi<-coef_db_hi["bis","Std. Error"]
slope_est_db_lo<-coef_db_lo["bis","Estimate"]
slope_se_db_lo<-coef_db_lo["bis","Std. Error"]

slope_sd_db_hi<-slope_est_db_hi*sqrt(hi_count)
slope_sd_db_lo<-slope_est_db_lo*sqrt(lo_count)

tsum.test(mean.x=slope_est_db_hi,s.x=slope_sd_db_hi, n.x=hi_count,
          mean.y=slope_est_db_lo,s.y=slope_sd_db_lo, n.y=hi_count)
