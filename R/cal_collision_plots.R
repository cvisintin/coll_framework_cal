require(ggplot2)
require(reshape2)
require(foreach)

load("output/cal_coll_glm_1000")

plotPal <- c("#b3de69")

invcloglog <- function (x) {1-exp(-exp(x))}

occ <- foreach(i = 1:length(models1000), .combine=rbind) %do% {
  data.frame(x=models1000[[i]][["data"]][["deer"]], y=invcloglog(cbind(1,log(models1000[[i]][["data"]][["deer"]]),mean(log(models1000[[i]][["data"]][["tvol"]])),mean((log(models1000[[i]][["data"]][["tvol"]]))*(log(models1000[[i]][["data"]][["tvol"]]))),mean(log(models1000[[i]][["data"]][["tspd"]]))) %*% models1000[[i]][["coefs"]]), col=rep(i, each=length(models1000[[i]][["data"]][["deer"]])))
}

tiff('figs/cal_occ.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(occ, aes(x=x,y=y,group=col)) +
  geom_line(size=0.2, col=plotPal, alpha=0.1) +
  #geom_smooth(size=0.8, se=FALSE, col="black", aes(group=1), n=10000) +
  ylab("Likelihood of Collision") +
  xlab("Likelihood of Species Occurrence") +
  theme(legend.position="none") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.5,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1)) +
  scale_y_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1)) #+
  #guides(colour=FALSE)
dev.off()


tvol <- foreach(i = 1:length(models1000), .combine=rbind) %do% {
  data.frame(x=models1000[[i]][["data"]][["tvol"]], y=invcloglog(cbind(1,mean(log(models1000[[i]][["data"]][["deer"]])),log(models1000[[i]][["data"]][["tvol"]]),(log(models1000[[i]][["data"]][["tvol"]]))*(log(models1000[[i]][["data"]][["tvol"]])),mean(log(models1000[[i]][["data"]][["tspd"]]))) %*% models1000[[i]][["coefs"]]), col=rep(i, each=length(models1000[[i]][["data"]][["tvol"]])))
}

tiff('figs/cal_tvol.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(tvol, aes(x=x/1000,y=y,group=col)) +
  geom_line(size=0.2, col=plotPal, alpha=0.1) +
  #geom_smooth(size=0.8, se=FALSE, col="black", aes(group=1), n=10000) +
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
  scale_x_continuous(breaks=seq(0,25,by=5), expand = c(0, 0), lim=c(0,25)) +
  scale_y_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1)) #+
  #guides(colour=FALSE)
dev.off()


tspd <- foreach(i = 1:length(models1000), .combine=rbind) %do% {
  data.frame(x=models1000[[i]][["data"]][["tspd"]], y=invcloglog(cbind(1,mean(log(models1000[[i]][["data"]][["deer"]])),mean(log(models1000[[i]][["data"]][["tvol"]])),mean((log(models1000[[i]][["data"]][["tvol"]]))*(log(models1000[[i]][["data"]][["tvol"]]))),log(models1000[[i]][["data"]][["tspd"]])) %*% models1000[[i]][["coefs"]]), col=rep(i, each=length(models1000[[i]][["data"]][["tspd"]])))
}

tiff('figs/cal_tspd.tif', pointsize = 12, compression = "lzw", res=300, width = 900, height = 900)
ggplot(tspd, aes(x=x*1.6,y=y,group=col)) +
  geom_line(size=0.2, col=plotPal, alpha=0.1) +
  #geom_smooth(size=0.8, se=FALSE, col="black", aes(group=1), n=10000) +
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
  scale_x_continuous(breaks=seq(40,110,by=10), expand = c(0, 0), lim=c(40,110)) +
  scale_y_continuous(breaks=seq(0,1,by=.1), expand = c(0, 0), lim=c(0,1)) #+
  #guides(colour=FALSE)
dev.off()
