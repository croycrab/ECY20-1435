#Manuscript ID: ECY20-1435
#Contents: Figure 2--interaction plots for plant biomass and survival and arthropod abundance

source("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/R Code/ECY20-1435_utilities.R")

#plant biomass
mod.biomass<-lmer(log(biomass)~precip_space*precip_time+age+(1|block)+(1|id)+(1|site),data=data.biomass,subset = biomass>0 & age>0)
bio1<-ggpredict(mod.biomass,type="fe",terms = c("precip_time","precip_space[all]"),back.transform =T)
names(bio1)[1]<-"precip_time"
names(bio1)[2]<-"biomass"
names(bio1)[6]<-"precip_space"
bio1$precip_space<-as.numeric(levels(bio1$precip_space)[bio1$precip_space])
filt1<-as.data.frame(vector(length = 21))
filt1$precip_space<-sort(unique(data.biomass$precip_space))[1:21]
for(i in sort(unique(data.biomass$precip_space))[1:21]){
  filt1$precip_time_max[which(filt1$precip_space == i)]<-max(data.biomass[which(data.biomass$precip_space==i & data.biomass$biomass>0),"precip_time"],na.rm = T)
}
filt1$precip_space<-round(filt1$precip_space,digits=2)
bio1<-bio1%>%
  left_join(filt1[,c("precip_space","precip_time_max")],by="precip_space")
bio1$keep<-with(bio1,ifelse(precip_time>precip_time_max,0,1))
bio1<-subset(bio1,keep==1)
bio.points<-subset(data.biomass, biomass>0 & age>0)
bio.points$predict<-predict(mod.biomass, newdata = bio.points)

p.bio.int<-ggplot(data=bio.points, aes(x=precip_time,y=exp(predict),group=precip_space)) +  geom_point(data=bio.points, aes(x=precip_time,y=exp(predict),color=precip_space,fill=precip_space),position = position_jitter(width = .15,height = 0.1),size=1,alpha=.9)+
  geom_path(inherit.aes = F,data=bio1,aes(x=precip_time,y=biomass,color=precip_space,group=precip_space),size=.65)+
  theme_manuscript+  
  theme(axis.line = element_blank())+
  theme(legend.position = c(.2,.875),legend.text = element_text(size=10))+
  scale_color_gradient(limits=c(26.5,91.7),low="red",high = "blue","Source Precipitation",guide = guide_colorbar(direction = "horizontal",title.position = "top"))+
  scale_fill_gradient(limits=c(26.5,91.7),low="red",high = "blue",guide = "none")+
  labs(x="",y="Biomass (g)")+
  annotate("text",x = 31, y = 1380, label = "CD*, Dr***, CDxDr**")
p.bio.int

#plant survival
mod.survival.lm<-lm(perc_survive~precip_time_lag*precip_space+garden,data=data.survival2)
surv1<-ggemmeans(mod.survival.lm,type="fe",terms = c("precip_time_lag","precip_space[all]"))
names(surv1)[1]<-"precip_time_lag"
names(surv1)[2]<-"perc_survive"
names(surv1)[6]<-"precip_space"
surv1$precip_space<-as.numeric(levels(surv1$precip_space)[surv1$precip_space])

p.surv.int<-ggplot(data=data.survival2, aes(x=precip_time_lag,y=perc_survive,group=precip_space)) +
  geom_point(data=data.survival2, aes(x=precip_time_lag,y=perc_survive,color=precip_space,fill=precip_space),position = position_jitter(width = .15,height = 0.01),size=1,alpha=.9)+
  geom_path(inherit.aes = F,data=surv1,aes(x=precip_time_lag,y=perc_survive,color=precip_space,group=precip_space),size=.65)+
  theme_manuscript+
  theme(axis.line = element_blank())+
  theme(legend.position = "none")+
  scale_color_gradient(limits=c(26.5,91.7),low="red",high = "blue","Source Precipitation")+
  scale_fill_gradient(limits=c(26.5,91.7),low="red",high = "blue",guide = "none")+
  labs(x="",y="Annual Survival")+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
  annotate("text",x = 31, y = .10, label = "CD**, Dr**, CDxDr***")
p.surv.int

#arthropod totals
#totals
mod.totals<-lmer(log(total)~precip_space*precip_time+age+(1|block)+(1|id)+(1|site),data=data.bugs,subset = biomass > 0 & total > 0)
tot1<-ggpredict(mod.totals,type="fe",terms = c("precip_time[10.668:43.942]","precip_space[all]"))
names(tot1)[1]<-"precip_time"
names(tot1)[2]<-"arthropod_total"
names(tot1)[6]<-"precip_space"
tot1$precip_space<-as.numeric(levels(tot1$precip_space)[tot1$precip_space])
filt4<-as.data.frame(vector(length = 21))
filt4$precip_space<-sort(unique(data.bugs$precip_space))[1:21]
for(i in sort(unique(data.bugs$precip_space))[1:21]){
  filt4$precip_time_max[which(filt4$precip_space == i)]<-max(data.bugs[which(data.bugs$precip_space==i & data.bugs$biomass>0 & data.bugs$total > 0),"precip_time"])
}
filt4$precip_space<-round(filt4$precip_space,digits=2)
tot1<-tot1%>%
  left_join(filt4[,c("precip_space","precip_time_max")],by="precip_space")
tot1$keep<-with(tot1,ifelse(precip_time>precip_time_max,0,1))
tot1<-subset(tot1,keep==1)
data.bugs2<-subset(data.bugs,biomass > 0 & total > 0)
bug.points<-data.bugs2
bug.points$predict2<-predict(mod.totals, newdata = bug.points)

p.tot.int<-ggplot(data=bug.points, aes(x=precip_time,y=exp(predict2),group=precip_space)) +
  geom_point(data=bug.points, aes(x=precip_time, y=exp(predict2),color=precip_space,group=precip_space),position = position_jitter(width = .15,height = 0.01),size=1,alpha=.9)+
  geom_path(inherit.aes = F,data=tot1,aes(x=precip_time,y=arthropod_total,color=precip_space,group=precip_space),size=.65)+
  theme_manuscript+
  theme(legend.position = "none")+
  theme(axis.line = element_blank())+
  scale_color_gradient(limits=c(26.5,91.7),low="red",high = "blue","Source Precipitation",guide = guide_colorbar(direction = "horizontal",title.position = "top"))+
  scale_fill_gradient(limits=c(26.5,91.7),low="red",high = "blue",guide = "none")+
  labs(x="Annual Precipitation (cm)",y=expression(paste('Annual Arthropod Yield (','#*',plant^-1,yr^-1,')',sep='')))+
  annotate("text",x = 20.5, y = 210, label =expression(paste("CD"^"MS",","," Dr***, CDxDr"^"NS",sep = " ")))
p.tot.int  

ggarrange(p.bio.int,p.surv.int, p.tot.int, ncol = 1,nrow = 3, align = "hv",common.legend = T,legend = "top")
