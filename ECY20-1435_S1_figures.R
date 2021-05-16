#Manuscript ID: ECY20-1435
#Contents: Supplementary Materials (S1) -- latitudinal variation in wet vs dry season

source("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/R Code/ECY20-1435_utilities.R")

#prepare data
data.climate2.cold<-subset(data.climate2,month %in% c(1:4,10:12))
data.climate2.warm<-subset(data.climate2,month %in% c(5:9))

data.climate2.coldyear<-data.climate2.cold%>%
  group_by(site,year)%>%
  summarise(ppt=sum(ppt),tmean=mean(tmean))
data.climate2.coldsite<-data.climate2.coldyear%>%
  group_by(site)%>%
  summarise(ppt=mean(ppt),tmean=mean(tmean))
data.climate2.coldsite<-data.climate2.coldsite%>%
  left_join(data.sites[,c("site","lat")],by="site")

data.climate2.warmyear<-data.climate2.warm%>%
  group_by(site,year)%>%
  summarise(ppt=sum(ppt),tmean=mean(tmean))
data.climate2.warmsite<-data.climate2.warmyear%>%
  group_by(site)%>%
  summarise(ppt=mean(ppt),tmean=mean(tmean))
data.climate2.warmsite<-data.climate2.warmsite%>%
  left_join(data.sites[,c("site","lat")],by="site")

data.climate2.coldsite$season<-"cold"
data.climate2.warmsite$season<-"warm"

data.climate2_seasons<-rbind(data.climate2.coldsite,data.climate2.warmsite)

data.climate2.month<-data.climate2%>%
  group_by(site,month)%>%
  summarise(ppt=mean(ppt),tmean=mean(tmean))
data.climate2.month<-data.climate2.month%>%
  left_join(data.sites[,c("site","lat")],by="site")

#plot
p6<-ggplot(data.climate2_seasons,aes(y=ppt/10,x=lat,group=season,color=season))+
  geom_point()+
  theme_manuscript+
  geom_smooth(method="lm")+
  scale_color_manual("Season",label=c("warm" = "Summer","cold" = "Winter"),values = c("warm" = "red","cold" = "blue"))+
  labs(y="Precipitation (cm)",x="Latitude")+
  theme(legend.position = "top")

p7<-ggplot(data.climate2.month,aes(y=ppt/10,x=month,color=lat,group=lat))+
  geom_point()+
  theme_manuscript+
  geom_smooth(method="lm",formula = y ~x+I(x^2),se=F)+
  scale_color_gradient("Latitude",low="red",high = "blue")+
  labs(y="Precipitation (cm)",x="Month")+
  theme(legend.position = "top")+
  scale_x_continuous(breaks = pretty_breaks(n = 12))

ggarrange(p6,p7,nrow=1,align = "hv",labels=c("(a)","(b)"))

#relative humidity vs precipitation from weather station near common garden
data.climate3<-subset(data.climate3,year != 1900)
data.climate3$dry_temp<-as.numeric(data.climate3$dry_temp)
data.climate3$wet_temp<-as.numeric(data.climate3$wet_temp)
data.climate3$RH<-as.numeric(data.climate3$RH)
data.climate3$precip<-as.numeric(data.climate3$precip)

data.climate3_daily<-data.climate3%>%
  group_by(year,month,day)%>%
  summarise(RH=mean(RH,na.rm=T),dry_temp=mean(dry_temp,na.rm=T),wet_temp=mean(wet_temp,na.rm=T),precip=sum(precip,na.rm=T))

data.climate3_daily$event<-with(data.climate3_daily,ifelse(precip>0,1,0))
data.climate3_daily$event2<-with(data.climate3_daily,sequence(rle(as.character(event))$lengths))
data.climate3_monthly<-data.climate3_daily%>%
  group_by(year,month)%>%
  summarise(RH=mean(RH,na.rm=T),dry_temp=mean(dry_temp,na.rm=T),wet_temp=mean(wet_temp,na.rm=T),precip=sum(precip,na.rm=T),event=sum(event,na.rm = T))

summary(lm(RH~precip,data=data.climate3_monthly,na.action = na.exclude))
p<-ggplot(data=data.climate3_monthly,aes(x=precip,y=RH/100))+
  geom_point()+
  geom_smooth(method = "lm",se=T,color="red",size=.5,linetype="dashed")+
  theme_manuscript+
  labs(x="Monthly Precipitation (cm)",y="Monthly Relative Humidity")+
  annotate("text", x = 15, y = .55, label = expression(paste(italic("P")," = 0.055")),hjust = 1.25, vjust = 1.25)+
  annotate("text", x = 15, y = .54, label = expression(paste(italic("R")^2," = 0.038")),hjust = 1.25, vjust = 1.25)+
  scale_y_continuous(labels = scales::percent_format(accuracy = 1))
p
