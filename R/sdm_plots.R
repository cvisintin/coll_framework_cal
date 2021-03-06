require(ggplot2)
require(gbm)
require(dismo)

load("output/vic_brt")
load("output/cal_brt")

#plot effect of light for kangaroos
light <- plot.gbm(kang.brt, i.var="LIGHT",return.grid=TRUE, type="response")
colnames(light) <- c("x","y")

tiff('figs/vic_light.tif', pointsize = 11, compression = "lzw", res=300, width = 900, height = 900)
ggplot(light,aes(x=x,y=y)) + 
  geom_line(size=0.3) + 
  ylab("Relative Occurrence") + 
  xlab("Artificial Light (relative)") + 
  theme_bw() + 
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.8,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(expand = c(0, 0), lim=c(0,65)) +
  annotate("text",  x=.05*max(light$x), y=.01*max(light$y), label = "Kangaroos", hjust=0)
dev.off()

#plot effect of elevation for kangaroos
elev <- plot.gbm(kang.brt, i.var="ELEV",return.grid=TRUE, type="response")
colnames(elev) <- c("x","y")

tiff('figs/vic_elev.tif', pointsize = 11, compression = "lzw", res=300, width = 900, height = 900)
ggplot(elev,aes(x=x,y=y)) +
  geom_line(size=0.3) +
  ylab("Relative Occurrence") +
  xlab("Elevation (m above sea level)") +
  theme_bw() + 
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.8,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(expand = c(0, 0), lim=c(0,1000)) +
  annotate("text",  x=.05*max(elev$x), y=.01*max(elev$y), label = "Kangaroos", hjust=0)
dev.off()

#plot effect of vegetation greenness for kangaroos
green <- plot.gbm(kang.brt, i.var="GREEN",return.grid=TRUE, type="response")
colnames(green) <- c("x","y")

tiff('figs/vic_green.tif', pointsize = 11, compression = "lzw", res=300, width = 900, height = 900)
ggplot(green,aes(x=x,y=y)) + 
  geom_line(size=0.3) + 
  ylab("Relative Occurrence") + 
  xlab("Seasonal Change in Vegetation Greenness") + 
  theme_bw() + 
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.8,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(expand = c(0, 0), lim=c(-0.1,0.5)) +
  annotate("text",  x=.05*max(green$x), y=.01*max(green$y), label = "Kangaroos", hjust=0)
dev.off()



#plot effect of slope for deer
slope <- plot.gbm(deer.brt, i.var="SLOPE",return.grid=TRUE, type="response")
colnames(slope) <- c("x","y")

tiff('figs/cal_slope.tif', pointsize = 11, compression = "lzw", res=300, width = 900, height = 900)
ggplot(slope,aes(x=x,y=y)) + 
  geom_line(size=0.3) + 
  ylab("Relative Occurrence") + 
  xlab("Slope (percent)") + 
  theme_bw() + 
  theme(legend.key = element_blank(), legend.position="none") +
  theme(plot.margin=unit(c(.5,.8,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(expand = c(0, 0), lim=c(0,50)) +
  annotate("text",  x=.05*max(slope$x), y=.01*max(slope$y), label = "Mule Deer", hjust=0)
dev.off()

#plot effect of light for deer
light <- plot.gbm(deer.brt, i.var="LIGHT",return.grid=TRUE, type="response")
colnames(light) <- c("x","y")

tiff('figs/cal_light.tif', pointsize = 11, compression = "lzw", res=300, width = 900, height = 900)
ggplot(light,aes(x=x,y=y)) + 
  geom_line(size=0.3) + 
  ylab("Relative Occurrence") + 
  xlab("Artificial Light (relative)") + 
  theme_bw() + 
  theme(legend.key = element_blank(), legend.position="none") +
  theme(plot.margin=unit(c(.5,.8,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(expand = c(0, 0), lim=c(0,65)) +
  annotate("text",  x=.05*max(light$x), y=.01*max(light$y), label = "Mule Deer", hjust=0)
dev.off()

#plot effect of greenness for deer
green <- plot.gbm(deer.brt, i.var="GREEN",return.grid=TRUE, type="response")
colnames(green) <- c("x","y")

tiff('figs/cal_green.tif', pointsize = 11, compression = "lzw", res=300, width = 900, height = 900)
ggplot(green,aes(x=x,y=y)) + 
  geom_line(size=0.3) + 
  ylab("Relative Occurrence") + 
  xlab("Seasonal Change in Vegetation Greenness") + 
  theme_bw() + 
  theme(legend.key = element_blank()) +
  theme(plot.margin=unit(c(.5,.8,.1,.1),"cm")) +
  theme(axis.title.x = element_text(margin=unit(c(.3,0,0,0),"cm"))) +
  theme(axis.title.y = element_text(margin=unit(c(0,.3,0,0),"cm"))) +
  theme(panel.grid.major = element_line(size=0.1),panel.grid.minor = element_line(size=0.1)) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(expand = c(0, 0), lim=c(-0.3,0.3)) +
  annotate("text",  x=.05*max(green$x), y=.01*max(green$y), label = "Mule Deer", hjust=0)
dev.off()
