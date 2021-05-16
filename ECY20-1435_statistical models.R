#Manuscript ID: ECY20-1435
#Contents: Statistical Models

source("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/R Code/ECY20-1435_utilities.R")

#decoupling age and precip
data.climate.sub<-subset(data.climate,garden == "5pop")
with(data.climate.sub,cor(precip_time, age, method = c("pearson")))
with(data.climate,cor(precip_time, age, method = c("pearson")))

#BIOMASS
mod.biomass<-lmer(log(biomass)~precip_space*precip_time+age+(1|block)+(1|id)+(1|site),data=data.biomass,subset = biomass>0 & age>0)
Anova(mod.biomass,type = 3)
summary(mod.biomass)

#plant survival
#Cox Regression Approach
mod.survival.cox<-coxph(Surv(age-1,age,mortality)~precip_space*precip_time_lag+garden,data = data.survival)
zph<-cox.zph(mod.survival.cox)
zph
mod.survival.cox
plot_model(mod.survival.cox,type="pred",terms = c("precip_time_lag","precip_space"))

#GLMER approach
mod.survival.glmer<-glmer(mortality~precip_space*precip_time_lag+age+garden+(1|site)+(1|id),family = "binomial",data=data.survival)
Anova(mod.survival.glmer,type = 3)
summary(mod.survival.glmer)
plot_model(mod.survival.glmer,type = "pred",terms = c("precip_time_lag","precip_space"),show.data = F,gg=T)

#Percent survival approach 
mod.survival.lm<-lm(perc_survive~precip_time_lag*precip_space+garden,data=data.survival2)
Anova(mod.survival.lm,type = 3)
summary(mod.survival.lm)
plot_model(mod.survival.lm,type = "pred",terms = c("precip_time_lag","precip_space"),show.data = T,gg=T)

#arthropod totals
mod.totals<-lmer(log(total)~precip_space*precip_time+age+(1|block)+(1|id)+(1|site),data=data.bugs,subset = biomass > 0 & total > 0)
Anova(mod.totals,type = 3)
summary(mod.totals)

#arthropod community composition
mod.perm<-adonis(log(data.bugs[,9:17]+1)~precip_space*precip_time,method = "bray",data=data.bugs,permutations = 1000,strata = as.factor(data.bugs$age))
mod.perm

