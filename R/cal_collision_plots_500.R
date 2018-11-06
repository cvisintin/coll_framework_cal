require(ggplot2)
require(reshape2)
require(foreach)
require(data.table)
require(png)
require(grid)

load("output/cal_coll_glm_500")
load("output/cal_coll_model_data_500")
load("output/cal_coll_cor_500")
load("output/cal_coll_cor_250")

invcloglog <- function (x) {1-exp(-exp(x))}

occ.range <- seq(0,1,1/(nrow(data)+1))[-c(1,length(seq(0,1,1/(nrow(data)+1))))]

occ.fit <- predict.glm(coll.glm,data.frame(deer=occ.range,tvol=mean(data$tvol),tspd=mean(data$tspd),length=1),type="response",se.fit=TRUE)
occ <- data.frame(x=occ.range,y=occ.fit[["fit"]],ymin=occ.fit[["fit"]]-1.96*occ.fit[["se.fit"]],ymax=occ.fit[["fit"]]+1.96*occ.fit[["se.fit"]])

#### expected annual collisions across the entire network ####
#occ <- data.frame(x=occ.range,y=(occ.fit[["fit"]])*sum(data$length)/10,ymin=occ.fit[["fit"]]*sum(data$length)/10-1.96*occ.fit[["se.fit"]]*sum(data$length)/10,ymax=occ.fit[["fit"]]*sum(data$length)/10+1.96*occ.fit[["se.fit"]]*sum(data$length)/10)
####

tiff('figs/cal_occ_500.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(occ, aes(x=x,y=y,ymin=ymin,ymax=ymax)) +
  geom_line(size=0.2) +
  geom_ribbon(alpha=0.3) +
  ylab("") +
  xlab("Likelihood of Species Occurrence") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1)) +
  annotate("text",  x=.05*max(occ$x), y=max(occ$y), label = "Mule Deer", hjust=0)
dev.off()


tvol.range <- seq(0,40000,40000/(nrow(data)+1))[-c(1,length(seq(0,40000,40000/(nrow(data)+1))))]

tvol.fit <- predict.glm(coll.glm,data.frame(deer=mean(data$deer),tvol=tvol.range,tspd=mean(data$tspd),length=1),type="response",se.fit=TRUE)
tvol <- data.frame(x=tvol.range,y=tvol.fit[["fit"]],ymin=tvol.fit[["fit"]]-1.96*tvol.fit[["se.fit"]],ymax=tvol.fit[["fit"]]+1.96*tvol.fit[["se.fit"]])

tiff('figs/cal_tvol_500.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(tvol, aes(x=x/1000,y=y,ymin=ymin,ymax=ymax)) +
  geom_line(size=0.2) +
  geom_ribbon(alpha=0.3) +
  ylab("") +
  xlab("Traffic Volume (1000 vehicles/day)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(breaks=seq(0,40,by=5), expand = c(0, 0), lim=c(0,40)) +
  annotate("text",  x=(.05*max(tvol$x))/1000, y=.1*max(tvol$y), label = "Mule Deer", hjust=0)
dev.off()


tspd.range <- seq(0,110,110/(nrow(data)+1))[-c(1,length(seq(0,110,110/(nrow(data)+1))))]

tspd.fit <- predict.glm(coll.glm,data.frame(deer=mean(data$deer),tvol=mean(data$tvol),tspd=tspd.range,length=1),type="response",se.fit=TRUE)
tspd <- data.frame(x=tspd.range,y=tspd.fit[["fit"]],ymin=tspd.fit[["fit"]]-1.96*tspd.fit[["se.fit"]],ymax=tspd.fit[["fit"]]+1.96*tspd.fit[["se.fit"]])

tiff('figs/cal_tspd_500.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(tspd, aes(x=x,y=y,ymin=ymin,ymax=ymax)) +
  geom_line(size=0.2) +
  geom_ribbon(alpha=0.3) +
  ylab("") +
  xlab("Traffic Speed (km/hour)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(breaks=seq(0,110,by=10), expand = c(0, 0), lim=c(0,110)) +
  annotate("text",  x=45, y=max(tspd$y), label = "Mule Deer", hjust=0)
dev.off()



tiff('figs/cal_coll_cor_500.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(cal.cor.df.1000,aes(x=x,y=y,group=as.factor(n))) +
  geom_line(colour=c("grey50"),size=0.2) +
  ylab("Moran's I") +
  xlab("Distance (km)") +
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype=2, size=0.3) +
  theme(text = element_text(size = 10), axis.text=element_text(size=6)) +
  scale_x_continuous(breaks=seq(1, 20, 1)) +
  annotate("text",  x=1, y=max(cal.cor.df.1000$y), label = "Mule Deer", hjust=0)
dev.off()

tiff('figs/cal_coll_cor_250.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(cal.cor.df.250,aes(x=x/4,y=y,group=as.factor(n))) +
  geom_line(colour=c("grey50"),size=0.2) +
  ylab("Moran's I") +
  xlab("Distance (km)") +
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype=2, size=0.3) +
  theme(text = element_text(size = 10), axis.text=element_text(size=6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_continuous(breaks=seq(.25, 5, .25)) +
  annotate("text",  x=.25, y=max(cal.cor.df.250$y), label = "Mule Deer", hjust=0)
dev.off()


################For Vic BioConf talk############################

tspd <- data.frame(x=seq(0.01,75,0.1), y=invcloglog(cbind(1,mean(log(data[,deer])),mean(log(data[,tvol])),mean((log(data[,tvol]))*(log(data[,tvol]))),log(tspd.range)) %*% coef(coll.glm)[1:5]))

tspd.fit <- predict.glm(coll.glm,data.frame(deer=mean(data$deer),tvol=mean(data$tvol),tspd=seq(0.01,75,0.1)),type="response",se.fit=TRUE)
tspd <- data.frame(x=seq(0.01,75,0.1),y=tspd.fit[["fit"]],ymin=tspd.fit[["fit"]]-1.96*tspd.fit[["se.fit"]],ymax=tspd.fit[["fit"]]+1.96*tspd.fit[["se.fit"]])

tspd$y <- (tspd$y/(data[coll==1,.N]/nrow(data)))/nrow(data)

pdf('/home/casey/Research/Projects/VicBioConf/graphics/tspd2.pdf', pointsize = 16)
ggplot(tspd, aes(x=x*1.6,y=y)) +
  geom_line(size=0.2) +
  ylab("Relative Collision Rate") +
  xlab("Traffic Speed (km/hour)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 16)) +
  scale_x_continuous(breaks=seq(0,120,by=20), expand = c(0, 0), lim=c(0,120)) #+
dev.off()

###### alternative based on predictions for phd talk######

occ.range <- seq(0,1,1/(nrow(data)+1))[-c(1,length(seq(0,1,1/(nrow(data)+1))))]

occ.fit <- predict.glm(coll.glm,data.frame(deer=occ.range,tvol=mean(data$tvol),tspd=mean(data$tspd),length=1),type="response",se.fit=TRUE)
occ <- data.frame(x=occ.range,y=occ.fit[["fit"]],ymin=occ.fit[["fit"]]-1.96*occ.fit[["se.fit"]],ymax=occ.fit[["fit"]]+1.96*occ.fit[["se.fit"]])

png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/cal_occ.png', pointsize = 16, res=300, width = 1000, height = 900)
ggplot(occ, aes(x=x,y=y,ymin=ymin,ymax=ymax)) +
  geom_line(size=0.8, colour='#1e501f') +
  geom_ribbon(alpha=0.3) +
  ylab("RELATIVE COLLISION RATE") +
  xlab("LIKELIHOOD OF SPECIES OCCURRENCE") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1))
dev.off()


tvol.range <- seq(0,40000,40000/(nrow(data)+1))[-c(1,length(seq(0,40000,40000/(nrow(data)+1))))]

tvol.fit <- predict.glm(coll.glm,data.frame(deer=mean(data$deer),tvol=tvol.range,tspd=mean(data$tspd),length=1),type="response",se.fit=TRUE)
tvol <- data.frame(x=tvol.range,y=tvol.fit[["fit"]],ymin=tvol.fit[["fit"]]-1.96*tvol.fit[["se.fit"]],ymax=tvol.fit[["fit"]]+1.96*tvol.fit[["se.fit"]])

png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/cal_tvol.png', pointsize = 16, res=300, width = 1000, height = 900)
ggplot(tvol, aes(x=x/1000,y=y,ymin=ymin,ymax=ymax)) +
  geom_line(size=0.8, colour='#1e501f') +
  geom_ribbon(alpha=0.3) +
  ylab("RELATIVE COLLISION RATE") +
  xlab("TRAFFIC VOLUME (1000 VEHICLES/DAY)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,40,by=5), expand = c(0, 0), lim=c(0,40))
dev.off()


tspd.range <- seq(0,68.75,68.75/(nrow(data)+1))[-c(1,length(seq(0,68.75,68.75/(nrow(data)+1))))]

tspd.fit <- predict.glm(coll.glm,data.frame(deer=mean(data$deer),tvol=mean(data$tvol),tspd=tspd.range,length=1),type="response",se.fit=TRUE)
tspd <- data.frame(x=tspd.range,y=tspd.fit[["fit"]],ymin=tspd.fit[["fit"]]-1.96*tspd.fit[["se.fit"]],ymax=tspd.fit[["fit"]]+1.96*tspd.fit[["se.fit"]])

png('/home/casey/Research/Projects/PhD_Thesis/completion_talk/graphics/cal_tspd.png', pointsize = 16, res=300, width = 1000, height = 900)
ggplot(tspd, aes(x=x*1.6,y=y,ymin=ymin,ymax=ymax)) +
  geom_line(size=0.8, colour='#1e501f') +
  geom_ribbon(alpha=0.3) +
  ylab("RELATIVE COLLISION RATE") +
  xlab("TRAFFIC SPEED (KM/HOUR)") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 8)) +
  scale_x_continuous(breaks=seq(0,110,by=10), expand = c(0, 0), lim=c(0,110))
dev.off()
#############################################