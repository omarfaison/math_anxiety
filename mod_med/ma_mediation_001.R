library(mediation)

##follows https://library.virginia.edu/data/articles/introduction-to-mediation-analysis
data<-read.csv("ma.impulsivity.data.20200122.csv")

emxma_model<-lm(easymathtime ~ amas, data)
summary(emxma_model)

bisxma_model<-lm(bis ~ amas, data)
summary(bisxma_model)

emxma_bis_model<-lm(easymathtime ~ amas + bis, data)
summary(emxma_bis_model)

em_med_results<-mediate(bisxma_model, emxma_bis_model, treat='amas', mediator='bis', boot=T, sims=500)
summary(em_med_results)

hmxma_model<-lm(hardmathtime ~ amas, data)
mtdxma_model<-lm(mathtimediff ~ amas, data)

summary(emxma_model)
summary(hmxma_model)
summary(mtdxma_model)