#Manuscript ID: ECY20-1435
#Contents: Figure 3--arthropod community composition 

source("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/R Code/ECY20-1435_utilities.R")



#view percent of each guild
guilds<-c("Herb_sap"="H-sapfeeders","Non_incidental"="incidentals","Herb_Chew"="H-chewers","Herb_other"="H-other","Pred_hunting"="P-hunting","Pred_web"="P-web","Pred_other"="P-other","Detrit_"="detritivores","Omni"="omnivores")
for(i in 9:17){
  print(names(data.bugs)[i])
  print(round((sum(data.bugs[,i],na.rm = T)/sum(data.bugs[,9:17],na.rm = T))*100,digits = 2))
}

#model
mod.perm<-adonis(log(data.bugs[,9:17]+1)~precip_space*precip_time,method = "bray",data=data.bugs,permutations = 1000,strata = as.factor(data.bugs$age))
mod.perm

#figure
set.seed(145)
cap1<-capscale(log(data.bugs[,9:17]+1)~precip_space*precip_time+Condition(age),method = "bray",data=data.bugs)
plot(cap1,type=)
cap1
0.4102/(0.4102+0.0311+0.0145)
0.0311/(0.4102+0.0311+0.0145)
anova(cap1,by="terms",permu=200)
RsquareAdj(cap1)$adj.r.squared
sumofsq<-c((0.0750/5.7898),(0.3576/5.7898))
guilds<-c("Herb_sap"="H-sapfeeders","Non_incidental"="incidentals","Herb_Chew"="H-chewers","Herb_other"="H-other","Pred_hunting"="P-hunting","Pred_web"="P-web","Pred_other"="P-other","Detrit_"="detritivores","Omni"="omnivores")
treatments<-c("precip_space"="Source \nPrecip.***","precip_time"="Annual \nPrecip.***","precip_space:precip_time"="Source Precip. x \nAnnual Precip***")

ggplot(data = as.data.frame(cap1$CCA$v),aes(x=0,y=0,yend=CAP2/2,xend=CAP1/2))+
  geom_point(inherit.aes = F,data=as.data.frame(cap1$CCA$wa),aes(y=CAP2,x=CAP1), size=1,alpha=.5)+
  geom_segment(color="blue",size=.8)+
  geom_segment(inherit.aes = F,data = as.data.frame(cap1$CCA$biplot),aes(x=0,y=0,yend=CAP2/2,xend=CAP1/2),color="red",size=.8)+
  theme_manuscript+
  annotate("label",x=cap1$CCA$v[,1]/2,y=cap1$CCA$v[,2]/2,label=guilds,size=3,alpha=.75,color="blue")+
  annotate("label",x=cap1$CCA$biplot[,1]/2,y=cap1$CCA$biplot[,2]/2,label=treatments,size=3,alpha=.75,color="red")+
  lims(x=c(-.5,.22),y=c(-.5,.35))+
  labs(x="CAP1 [90%]",y="CAP2 [7%]")

