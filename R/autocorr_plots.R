require(ggplot2)

load("output/vic_brt_cor")
load("output/cal_brt_cor")

vic.cor.df <- cbind(vic.cor.df,"col"="kangaroo")
cal.cor.df <- cbind(cal.cor.df,"col"="deer")

auto.resid <- rbind(cal.cor.df, vic.cor.df)

ggplot(auto.resid,aes(x=x,y=y,group=col,shape=col)) +
  geom_line(colour=c("grey70"),size=.75) +
  geom_point(size=2.5) +
  ylab("Moran's I") +
  xlab("Distance (km)") +
  labs(shape = "Species") +
  theme_bw() +
  theme(legend.key = element_blank()) +
  #scale_shape_manual(values=shapes) +
  geom_hline(aes(yintercept=0), linetype=2) +
  scale_x_continuous(breaks=seq(1, 20, 1))

#Plot first 20 kms in 1 km bins
ggplot(vic.cor.df,aes(x=x,y=y)) + 
  geom_line(colour=c("grey70"),size=1) + 
  ylab("Moran's I") + 
  xlab("Distance (km)") + 
  theme_bw() + 
  theme(legend.key = element_blank()) +
  theme(text = element_text(size = 20)) +
  geom_hline(aes(yintercept=0), linetype=2) + 
  scale_x_continuous(breaks=seq(1, 20, 1))
