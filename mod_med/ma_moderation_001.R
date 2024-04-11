library(rempsyc)
library(effectsize)
library(flextable)
library(interactions)
library(tidyverse)

##follows https://cran.r-project.org/web/packages/rempsyc/vignettes/moderation.html
data<-read.csv("ma.impulsivity.data.20200122.csv")

mod_data<-select(data, amas, bis, easymathtime, hardmathtime, mathtimediff)
data2<-lapply(mod_data, scale) |> as.data.frame()

em_mod<-nice_mod(
  data=data2,
  response="easymathtime",
  predictor="amas",
  moderator="bis"
)
em_mod

em_slopes<-nice_slopes(data=data2,
                       response="easymathtime",
                       predictor="amas",
                       moderator="bis"
)
em_slopes

hm_mod<-nice_mod(
  data=data2,
  response="hardmathtime",
  predictor="amas",
  moderator="bis"
)
hm_mod

hm_slopes<-nice_slopes(data=data2,
                       response="hardmathtime",
                       predictor="amas",
                       moderator="bis"
)
hm_slopes

mtd_mod<-nice_mod(
  data=data2,
  response="mathtimediff",
  predictor="amas",
  moderator="bis"
)
mtd_mod

mtd_slopes<-nice_slopes(data=data2,
                       response="mathtimediff",
                       predictor="amas",
                       moderator="bis"
)
mtd_slopes

