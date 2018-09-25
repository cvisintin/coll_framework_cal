require(plyr)

####Collision rate to expected collisions simulations#######
segment_colls <- runif(250,0,2)


sim.data <- data.frame(
  rate=runif(250,.001,.2),
  rdlength=seq(.01,2.5,.01),
  years=rep(6,250)
)

number.coll <- with(sim.data, rate * rdlength * years)

number.coll.bin <- sapply(number.coll, function(x) ifelse(x>=1, 1, 0))

glm(rate~number.coll.bin, offset=log(rdlength*years),family=poisson,data=cbind(number.coll.bin,sim.data))

