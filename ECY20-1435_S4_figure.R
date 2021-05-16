#Manuscript ID: ECY20-1435
#Contents: Supplementary Materials (S4) -- latitudinal variation in soil composition

source("C:/Users/croyj/Dropbox/ARCA/Manuscripts/Arthropod Time Series/Finalized for Publication/R Code/ECY20-1435_utilities.R")

data.soil.w <- data.soil %>%
  mutate(thick = ifelse(hzdepb.r > 5, 50 - hzdept.r, 
                        hzdepb.r - hzdept.r)) %>%  
  group_by(cokey) %>%
  summarise(mukey=mukey,
            comppct.r=comppct.r,
            sand = round(weighted.mean(sand, thick, na.rm = TRUE),2),
            silt = round(weighted.mean(silt, thick, na.rm = TRUE),2),
            clay = round(weighted.mean(clay, thick, na.rm = TRUE),2),
            om = round(weighted.mean(om, thick, na.rm = TRUE),2),
            ksat = round(weighted.mean(ksat, thick, na.rm = TRUE),2),
            k = round(weighted.mean(kffact, thick, na.rm = TRUE),2),
            cec = round(weighted.mean(cec, thick, na.rm = TRUE),2),
            ph = round(weighted.mean(ph, thick),2),
            awc = round(weighted.mean(awc, thick),2))

data.soil.w2<-data.soil.w%>%
  group_by(mukey)%>%
  summarise(sand = weighted.mean(sand,comppct.r,na.rm=TRUE),
            silt = weighted.mean(silt,comppct.r,na.rm=TRUE),
            clay = weighted.mean(clay,comppct.r,na.rm=TRUE),
            om = weighted.mean(om,comppct.r,na.rm=TRUE),
            ksat = weighted.mean(ksat,comppct.r,na.rm=TRUE),
            k = weighted.mean(k,comppct.r,na.rm=TRUE),
            cec = weighted.mean(cec,comppct.r,na.rm=TRUE),
            ph = weighted.mean(ph,comppct.r,na.rm=TRUE),
            awc = weighted.mean(awc,comppct.r,na.rm=TRUE))

data<-data.soil.w2%>%
  left_join(data.mukey[,c("Site","lat","long","mukey")],by="mukey")

data$sand[is.nan(data$sand)]<-NA
data$silt[is.nan(data$silt)]<-NA
data$ksat[is.nan(data$ksat)]<-NA
data$k[is.nan(data$k)]<-NA
data$cec[is.nan(data$cec)]<-NA
data$ph[is.nan(data$ph)]<-NA
data$awc[is.nan(data$awc)]<-NA

#analyze and graph soil data
mod.sand<-lm(sand~lat,data)
summary(mod.sand)
g.sand<-ggplot(data,aes(y=sand,x=lat))+
  geom_point()+
  geom_smooth(method = "lm",color="black",lty=1)+
  theme_classic()+
  labs(y="% Sand",x="Latitude")+
  annotate("text",x=33.5, y=min(data$sand,na.rm = T),label=paste0("P = ",round(summary(mod.sand)$coefficients[2,4],3)))
g.sand

mod.silt<-lm(silt~lat,data)
summary(mod.silt)
g.silt<-ggplot(data,aes(y=silt,x=lat))+
  geom_point()+
  geom_smooth(method = "lm",color="black",lty=1)+
  theme_classic()+
  labs(y="% Silt",x="Latitude")+
  annotate("text",x=33.5, y=min(data$silt,na.rm = T),label=paste0("P = ",round(summary(mod.silt)$coefficients[2,4],3)))
g.silt

mod.clay<-lm(clay~lat,data)
summary(mod.clay)
g.clay<-ggplot(data,aes(y=clay,x=lat))+
  geom_point()+
  geom_smooth(method = "lm",color="black",lty=1)+
  theme_classic()+
  labs(y="% Clay",x="Latitude")+
  annotate("text",x=33.5, y=min(data$clay,na.rm = T),label=paste0("P = ",round(summary(mod.clay)$coefficients[2,4],3)))
g.clay

mod.om<-lm(om~lat,data)
summary(mod.om)
g.om<-ggplot(data,aes(y=om,x=lat))+
  geom_point()+
  geom_smooth(method = "lm",color="black",lty=1)+
  theme_classic()+
  labs(y="% Organic Matter",x="Latitude")+
  annotate("text",x=33.5, y=min(data$om,na.rm = T),label=paste0("P = ",round(summary(mod.om)$coefficients[2,4],3)))
g.om


mod.k<-lm(k~lat,data)
summary(mod.k)
g.k<-ggplot(data,aes(y=k,x=lat))+
  geom_point()+
  geom_smooth(method = "lm",color="black",lty=1)+
  theme_classic()+
  labs(y="Erodibility Factor (k)",x="Latitude")+
  annotate("text",x=33.5, y=min(data$k,na.rm = T),label=paste0("P = ",round(summary(mod.k)$coefficients[2,4],3)))
g.k

mod.cec<-lm(cec~lat,data)
summary(mod.cec)
g.cec<-ggplot(data,aes(y=cec,x=lat))+
  geom_point()+
  geom_smooth(method = "lm",color="black",lty=1)+
  theme_classic()+
  labs(y="Readily Exchangeable Cations",x="Latitude")+
  annotate("text",x=33.5, y=min(data$cec,na.rm = T),label=paste0("P = ",round(summary(mod.cec)$coefficients[2,4],3)))
g.cec

mod.ph<-lm(ph~lat,data)
summary(mod.ph)
g.ph<-ggplot(data,aes(y=ph,x=lat))+
  geom_point()+
  geom_smooth(method = "lm",color="black",lty=1)+
  theme_classic()+
  labs(y="pH",x="Latitude")+
  annotate("text",x=33.5, y=min(data$ph,na.rm = T),label=paste0("P = ",round(summary(mod.ph)$coefficients[2,4],3)))
g.ph

mod.ksat<-lm(ksat~lat,data)
summary(mod.ksat)
g.ksat<-ggplot(data,aes(y=ksat,x=lat))+
  geom_point()+
  geom_smooth(method = "lm",color="black",lty=1)+
  theme_classic()+
  labs(y="Water Flow (K-sat)",x="Latitude")+
  annotate("text",x=33.5, y=min(data$ksat,na.rm = T),label=paste0("P = ",round(summary(mod.ksat)$coefficients[2,4],3)))
g.ksat

mod.awc<-lm(awc~lat,data)
summary(mod.awc)
g.awc<-ggplot(data,aes(y=awc,x=lat))+
  geom_point()+
  geom_smooth(method = "lm",color="black",lty=1)+
  theme_classic()+
  labs(y="Water Storage CApacity (AWC)",x="Latitude")+
  annotate("text",x=33.5, y=min(data$awc,na.rm = T),label=paste0("P = ",round(summary(mod.awc)$coefficients[2,4],3)))
g.awc

g.soil<-ggarrange(g.sand,g.silt,g.clay,g.om,g.k,g.cec,g.ph,g.ksat,g.awc,nrow = 3,ncol = 3)
g.soil

#multivariate approach
x<-data[complete.cases(data[,2:10]),]
x2<-x[,c(2:10)]
pca.x<-prcomp(x2, scale = TRUE)
summary(pca.x)
library(factoextra)
fviz_eig(pca.x)
x.mod<-lm(pca.x$x[,1]~x$lat,data = x2)
summary(x.mod)
g.all<-ggplot(data=x2,aes(y=pca.x$x[,1],x=x$lat))+
  geom_point()+
  geom_smooth(method = "lm", color="black")+
  theme_classic()+
  labs(y="PCA1 [53.3%]",x="Latitude")+
  annotate("text",x=33.5, y=min(pca.x$x[,1],na.rm = T),label=paste0("P = ",round(summary(x.mod)$coefficients[2,4],3)))
g.all

pc.loading<-as.data.frame(pca.x$rotation)
pc.loading$trait<-rownames(pc.loading)
g.load<-ggbarplot(data=pc.loading,x="trait",y="PC1",orientation = "vertical", fill = "black",xlab = "")+
  theme_classic()+
  theme(axis.text.y =element_text(size=10))+
  scale_x_discrete(labels=c("Sand","Silt","Clay","OM","Ksat","K","CEC","pH","AWC"))+
  ylim(c(-.525,.525))+
  labs(y="PC1 [53.3%]")+
  geom_hline(yintercept = 0)+
  theme(plot.margin=unit(c(1,1,1,0), "cm"))

ggarrange(g.all,g.load,align = "h")