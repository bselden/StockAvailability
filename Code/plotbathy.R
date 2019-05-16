library(marmap)

dat <- getNOAA.bathy(-126,-118,30,50,res=1, keep=TRUE) #generates a csv with the depth data 

#plot(dat)
plot(dat, deepest.isobath=-200, 
     shallowest.isobath=0, step=200, lwd=0.1, drawlabel=TRUE, land=T)

# dat.m <- matrix(dat, nrow=nrow(dat))
# dimnames(dat.m) <- dimnames(dat)

