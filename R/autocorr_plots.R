require(ggplot2)

load("output/vic_brt_cor")
load("output/cal_brt_cor")

vic.cor.df <- cbind(vic.cor.df,"col"="kangaroo")
cal.cor.df <- cbind(cal.cor.df,"col"="deer")

auto.resid <- rbind(cal.cor.df, vic.cor.df)

tiff('figs/sac.tif', pointsize = 11, compression = "lzw", res=300, width = 1600, height = 900)
ggplot(auto.resid,aes(x=x,y=y,shape=col)) +
  geom_line(colour=c("grey70"),size=.3) +
  geom_point(size=1.0) +
  ylab("Moran's I") +
  xlab("Distance (km)") +
  #labs(shape = "Species") +
  theme_bw() +
  theme(legend.key = element_blank(), legend.position="none") +
  #scale_shape_manual(values=shapes) +
  geom_hline(aes(yintercept=0), linetype=2, size=0.3) +
  theme(text = element_text(size = 10)) +
  scale_x_continuous(breaks=seq(1, 20, 1))
dev.off()
