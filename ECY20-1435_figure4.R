#Manuscript ID: ECY20-1435
#Contents: Figure 4--effect size figures

source("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/R Code/ECY20-1435_utilities.R")


###prepare dataset
set.seed(12345)
#biomass
mod.biomass<-lmer(log(biomass)~precip_space*precip_time+age+(1|block)+(1|id)+(1|site),data=data.biomass,subset = biomass>0 & age>0)
bio1<-ggpredict(mod.biomass,type="fe",terms = c("precip_time[29.9,19.9]","precip_space[29.9,39.9]"),ci.lvl = 0.68)
names(bio1)[1]<-"precip_time"
names(bio1)[2]<-"biomass"
names(bio1)[6]<-"precip_space"
bio1$precip_space<-as.numeric(levels(bio1$precip_space)[bio1$precip_space])
# bio1$biomass_notlog<-exp(bio1$biomass)
rownames(bio1)<-c("dr","dr*cd","baseline","cd")
bl_bio<-bio1["baseline",]
bio1<-bio1[which(rownames(bio1)!="baseline"),]
bio1$treat<-as.factor(rownames(bio1))

#plant survival and life expectancy
mod.survival.lm<-lm(perc_survive~precip_time_lag*precip_space+garden,data=data.survival2)
surv1<-ggemmeans(mod.survival.lm,type="fe",terms = c("precip_time_lag[29.9,19.9]","precip_space[29.9,39.9]"),ci.lvl = 0.68)
names(surv1)[1]<-"precip_time_lag"
names(surv1)[2]<-"survival"
names(surv1)[6]<-"precip_space"
surv1$precip_space<-as.numeric(levels(surv1$precip_space)[surv1$precip_space])
surv1$LE<-log(.5)/log(surv1$survival)
rownames(surv1)<-c("dr","dr*cd","baseline","cd")
bl_surv<-surv1["baseline",]
bl_surv$le_conf.high<-with(bl_surv,log(.5)/log(bl_surv$conf.high))
bl_surv$le_conf.low<-with(bl_surv,log(.5)/log(bl_surv$conf.low))
surv1<-surv1[which(rownames(surv1)!="baseline"),]
surv1$treat<-as.factor(rownames(surv1))
surv1$prop_error<-with(surv1,(conf.high-survival)/survival)
surv1$le_conf.high<-with(surv1,log(.5)/log(surv1$conf.high))
surv1$le_conf.low<-with(surv1,log(.5)/log(surv1$conf.low))

#annual and lifetime arthropod total
mod.totals<-lmer(log(total)~precip_space*precip_time+age+(1|block)+(1|id)+(1|site),data=data.bugs,subset = biomass > 0 & total > 0)
tot1<-ggpredict(mod.totals,type="fe",terms = c("precip_time[29.9,19.9]","precip_space[29.9,39.9]"),ci.lvl = 0.68)
names(tot1)[1]<-"precip_time"
names(tot1)[2]<-"arthropod_total"
names(tot1)[6]<-"precip_space"
tot1$precip_space<-as.numeric(levels(tot1$precip_space)[tot1$precip_space])
rownames(tot1)<-c("dr","dr*cd","baseline","cd")
bl_tot<-tot1["baseline",]
tot1<-tot1[which(rownames(tot1)!="baseline"),]
tot1$treat<-as.factor(rownames(tot1))
tot1$prop_error<-with(tot1,((conf.high-arthropod_total))/arthropod_total)
tot1$lifetime_total<-tot1$arthropod_total*surv1$LE
bl_tot$lt_bl<-bl_tot$arthropod_total*bl_surv$LE
tot1$lt_conf.high2<-tot1$conf.high*surv1$le_conf.high
tot1$lt_conf.low2<-tot1$conf.low*surv1$le_conf.low
bl_tot$lt_conf.high2<-bl_tot$conf.high*bl_surv$le_conf.high
bl_tot$lt_conf.low2<-bl_tot$conf.low*bl_surv$le_conf.low

#effect sizes for each factor
surv1$eff.le<-(1-(surv1$LE/bl_surv$LE))*100
surv1$eff.s<-(1-(surv1$survival/bl_surv$survival))*100
tot1$eff.t<-(1-(tot1$arthropod_total/bl_tot$arthropod_total))*100
tot1$eff.lt<-(1-(tot1$lifetime_total/bl_tot$lt_bl))*100
bio2$eff.b<-(1-(bio2$biomass/bl_bio$biomass))*100
leaf1$eff.la<-(1-(leaf1$leaf_area/bl_leaf$leaf_area))*100
dens1$eff.d<-(1-(dens1$arthropod_density/bl_dens$arthropod_density))*100

#multiplicative risk estimates
tot.ann.mr<- 1-(tot1$eff.t[1]/100)-(tot1$eff.t[3]/100)+((tot1$eff.t[1]/100)*(tot1$eff.t[3])/100)
tot.lt.mr<-1-(tot1$eff.lt[1]/100)-(tot1$eff.lt[3]/100)+((tot1$eff.lt[1]/100)*(tot1$eff.lt[3])/100)
le.mr<-1-(surv1$eff.le[1]/100)-(surv1$eff.le[3]/100)+((surv1$eff.le[1]/100)*(surv1$eff.le[3])/100)
surv.mr<-1-(surv1$eff.s[1]/100)-(surv1$eff.s[3]/100)+((surv1$eff.s[1]/100)*(surv1$eff.s[3])/100)
bio.mr<-1-(bio1$eff.b[1]/100)-(bio1$eff.b[3]/100)+((bio1$eff.b[1]/100)*(bio1$eff.b[3])/100)
leaf.mr<-1-(leaf1$eff.la[1]/100)-(leaf1$eff.la[3]/100)+((leaf1$eff.la[1]/100)*(leaf1$eff.la[3])/100)
dens.mr<-1-(dens1$eff.d[1]/100)-(dens1$eff.d[3]/100)+((dens1$eff.d[1]/100)*(dens1$eff.d[3])/100)

###plot results

#plant biomass effect size
p.bio<-ggplot(data = bio1,aes(x=factor(bio1$treat, levels = c("cd","dr","cd*dr")),y=biomass))+
  geom_point(shape=15,size=3)+
  geom_errorbar(data = bio1,aes(ymin=conf.low, ymax=conf.high), width=.1)+
  theme_manuscript+
  theme(axis.line = element_blank())+
  theme(axis.text.x = element_text(size = 15,color = "black"))+
  geom_hline(yintercept = bl_bio[,2],color="black",linetype="solid")+
  annotate("rect",xmin=0.4,xmax = 3.6,ymin =bl_bio[,4],ymax = bl_bio[,5],alpha=.4,color="gray")+
  xlab("")+
  ylab(str_wrap("Biomass (g)",width = 20))+
  scale_x_discrete(labels=c("","",""))+
  scale_y_continuous(sec.axis = sec_axis(~ ((./bl_bio[,2])), name = "",labels=scales::percent_format(accuracy = 1),breaks = scales::pretty_breaks(n = 5)),limits = c(0,bl_bio[,5]+10))+
  annotate("segment",x= 2.85,xend =3.15,y= bio.mr*bl_bio$biomass,yend=bio.mr*bl_bio$biomass,col="orange",size=2)
p.bio

#life expectancy
p.le<-ggplot(data = surv1,aes(x=factor(surv1$treat, levels = c("cd","dr","cd*dr")),y=LE))+
  geom_point(shape=15,size=3)+
  geom_errorbar(data = surv1,aes(ymin=le_conf.low, ymax=le_conf.high), width=.1)+
  theme_manuscript+
  theme(axis.text.x = element_text(size = 14,color = "black"))+
  theme(axis.line = element_blank())+
  theme(axis.text.x = element_text(size = 15,color = "black"))+
  geom_hline(yintercept = bl_surv[,7],color="black",linetype="solid")+
  annotate("rect",xmin=0.4,xmax = 3.6,ymin =bl_surv[,9],ymax = bl_surv[,8],alpha=.4,color="gray")+  xlab("")+
  ylab("Median Survival Time (yr)")+
  scale_x_discrete(labels=str_wrap(c("cd" = "", "dr" = "","cd*dr" = ""),width = 10))+
  scale_y_continuous(sec.axis = sec_axis(~ ((./bl_surv[,7])), name = "",labels=scales::percent_format(accuracy = 1),breaks = scales::pretty_breaks(n = 5)),limits = c(0,30))+
  annotate("label",x=factor(surv1$treat, levels = c("cd","dr","cd*dr")),y=surv1$le_conf.low-1.15,label=round(surv1$LE,digits = 1))+
  annotate("label",x=3.3,y=le.mr*bl_surv$LE,label=round(le.mr*bl_surv$LE,digits=1))+
  annotate("segment",x= 2.85,xend =3.15,y= le.mr*bl_surv$LE,yend=le.mr*bl_surv$LE,col="orange",size=2)
p.le

#arthropod total
p.tot<-ggplot(data = tot1,aes(x=factor(tot1$treat, levels = c("cd","dr","cd*dr")),y=arthropod_total))+
  geom_point(shape=15,size=3)+
  geom_errorbar(data = tot1,aes(ymin=conf.low, ymax=conf.high), width=.1)+
  theme_manuscript+
  theme(axis.text.x = element_text(size = 14,color = "black"))+
  theme(axis.line = element_blank())+
  geom_hline(yintercept = bl_tot[,2],color="black",linetype="solid")+
  annotate("rect",xmin=0.4,xmax = 3.6,ymin =bl_tot[,4],ymax = bl_tot[,5],alpha=.4,color="gray")+  
  xlab("")+
  ylab("Annual Arthropod Yield")+
  scale_x_discrete(labels=str_wrap(c("cd" = "CD", "dr" = "Dr","cd*dr" = "CD + Dr"),width = 10))+
  scale_y_continuous(sec.axis = sec_axis(~ ((./bl_tot[,2])), name = "",labels=scales::percent_format(accuracy = 1),breaks = scales::pretty_breaks(n = 5)),limits = c(0,bl_tot[,5]*1.025))+
  annotate("segment",x= 2.85,xend =3.15,y= tot.ann.mr*bl_tot$arthropod_total,yend=tot.ann.mr*bl_tot$arthropod_total,col="orange",size=2)
p.tot

#lifetime totals
p.life.tot<-ggplot(data = tot1,aes(x=factor(tot1$treat, levels = c("cd","dr","cd*dr")),y=lifetime_total))+
  geom_point(shape=15,size=3)+
  geom_errorbar(data = tot1,aes(ymin=lt_conf.low2, ymax=lt_conf.high2), width=.1)+
  theme_manuscript+
  theme(axis.text.x = element_text(size = 14,color = "black"))+
  theme(axis.line = element_blank())+
  geom_hline(yintercept = bl_tot[,7],color="black",linetype="solid")+
  annotate("rect",xmin=0.4,xmax = 3.6,ymin =bl_tot[,9],ymax = bl_tot[,8],alpha=.4,color="gray")+  xlab("")+xlab("")+
  xlab("")+
  ylab("Lifetime Arthropod Yield")+
  scale_x_discrete(labels=str_wrap(c("cd" = "CD", "dr" = "Dr","cd*dr" = "CD + Dr"),width = 10))+
  scale_y_continuous(sec.axis = sec_axis(~ ((./bl_tot[,7])), name = "",labels=scales::percent_format(accuracy = 1),breaks = scales::pretty_breaks(n = 5)),limits = c(0,bl_tot[,8]*1.025))+
  annotate("segment",x= 2.85,xend =3.15,y= tot.lt.mr*bl_tot$lt_bl,yend=tot.lt.mr*bl_tot$lt_bl,col="orange",size=2)
p.life.tot

ggarrange(p.bio,p.le,p.tot,p.life.tot, ncol = 2,nrow = 2, align = "hv", labels = c("(a)","(b)","(c)","(d)"))
