require(ggplot2)
require(reshape2)
require(foreach)
require(png)
require(grid)

load("output/vic_coll_glm_500")
load("output/vic_coll_model_data_500")
load("output/vic_coll_cor_1000")
load("output/vic_coll_cor_250")

invcloglog <- function (x) {1-exp(-exp(x))}

occ.range <- seq(0,1,1/(nrow(data)+1))[-c(1,length(seq(0,1,1/(nrow(data)+1))))]

occ.fit <- predict.glm(coll.glm,data.frame(egk=occ.range,tvol=mean(data$tvol),tspd=mean(data$tspd),length=1),type="response",se.fit=TRUE)
occ <- data.frame(x=occ.range,y=occ.fit[["fit"]],ymin=occ.fit[["fit"]]-1.96*occ.fit[["se.fit"]],ymax=occ.fit[["fit"]]+1.96*occ.fit[["se.fit"]])

tiff('figs/vic_occ.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(occ, aes(x=x,y=y,ymin=ymin,ymax=ymax)) +
  geom_line(size=0.3) +
  geom_ribbon(alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Likelihood of Species Occurrence") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1)) +
  annotate("text",  x=.05*max(occ$x), y=max(occ$y), label = "Kangaroos", hjust=0)
dev.off()


tvol.range <- seq(0,40000,40000/(nrow(data)+1))[-c(1,length(seq(0,40000,40000/(nrow(data)+1))))]

tvol.fit <- predict.glm(coll.glm,data.frame(egk=mean(data$egk),tvol=tvol.range,tspd=mean(data$tspd),length=1),type="response",se.fit=TRUE)
tvol <- data.frame(x=tvol.range,y=tvol.fit[["fit"]],ymin=tvol.fit[["fit"]]-1.96*tvol.fit[["se.fit"]],ymax=tvol.fit[["fit"]]+1.96*tvol.fit[["se.fit"]])

tiff('figs/vic_tvol.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(tvol, aes(x=x/1000,y=y,ymin=ymin,ymax=ymax)) +
  geom_line(size=0.3) +
  geom_ribbon(alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Traffic Volume (1000 vehicles/day)") +
  theme(legend.position="none") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(breaks=seq(0,40,by=5), expand = c(0, 0), lim=c(0,40)) +
  annotate("text",  x=(.05*max(tvol$x))/1000, y=.1*max(tvol$y), label = "Kangaroos", hjust=0)
dev.off()


tspd.range <- seq(0,110,110/(nrow(data)+1))[-c(1,length(seq(0,110,110/(nrow(data)+1))))]

tspd.fit <- predict.glm(coll.glm,data.frame(egk=mean(data$egk),tvol=mean(data$tvol),tspd=tspd.range,length=1),type="response",se.fit=TRUE)
tspd <- data.frame(x=tspd.range,y=tspd.fit[["fit"]],ymin=tspd.fit[["fit"]]-1.96*tspd.fit[["se.fit"]],ymax=tspd.fit[["fit"]]+1.96*tspd.fit[["se.fit"]])

tiff('figs/vic_tspd.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(tspd, aes(x=x,y=y,ymin=ymin,ymax=ymax)) +
  geom_line(size=0.3) +
  geom_ribbon(alpha=0.3) +
  ylab("Likelihood of Collision") +
  xlab("Traffic Speed (km/hour)") +
  theme(legend.position="none") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(breaks=seq(0,110,by=10), expand = c(0, 0), lim=c(0,110)) +
  annotate("text",  x=45, y=max(tspd$y), label = "Kangaroos", hjust=0)
dev.off()



tiff('figs/vic_coll_cor_1000.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(vic.cor.df.1000,aes(x=x,y=y,group=as.factor(n))) +
  geom_line(colour=c("grey50"),size=0.2) +
  ylab("Moran's I") +
  xlab("Distance (km)") +
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype=2, size=0.3) +
  theme(text = element_text(size = 10), axis.text=element_text(size=6)) +
  scale_x_continuous(breaks=seq(1, 20, 1)) +
  annotate("text",  x=1, y=max(vic.cor.df.1000$y), label = "Kangaroos", hjust=0)
dev.off()


tiff('figs/vic_coll_cor_250.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(vic.cor.df.250,aes(x=x/4,y=y,group=as.factor(n))) +
  geom_line(colour=c("grey50"),size=0.2) +
  ylab("Moran's I") +
  xlab("Distance (km)") +
  theme_bw() +
  geom_hline(aes(yintercept=0), linetype=2, size=0.3) +
  theme(text = element_text(size = 10), axis.text=element_text(size=6)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 0)) +
  scale_x_continuous(breaks=seq(.25, 5, .25)) +
  annotate("text",  x=.25, y=max(vic.cor.df.250$y), label = "Kangaroos", hjust=0)
dev.off()