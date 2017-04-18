# show mesh optimization evolution
testkast=read.csv("secrun/pareto_test100.csv") #read #red
testdom=function(testobj,pob,pbest){
  test=matrix(rep(testobj,pop),ncol=2,byrow=T)
  bool=!any(apply(test>pbest,1,all))
}
pbest=cbind(testkast$X.1,testkast$X.2)
pop=48
nondkast=apply(pbest,1,function(x) testdom(x,pop,pbest))
pareto_kast=pbest[nondkast,]
pareto_log=pareto_kast
pareto_log[,1]=log10(pareto_log[,1])

png('mesh_opti.png',width=1200,height=900,units = 'px')
par(mfrow=c(2,3),cex=1.4, cex.axis=1.4, cex.lab=1.4, cex.main=1.2)
#1
plot(testkast$X.1[testkast$X.1<999],testkast$X.2[testkast$X.1<999],xlab='RMSE',ylab='Nodes',pch=3,lwd=2,col='darkorange',ylim=c(1000,1300),xlim=c(0.0004,0.0008), main="200 generations")
par(new=T)
plot(pareto_kast,xlab='',ylab='',pch=3,lwd=2,col='darkred',ylim=c(1000,1300),xlim=c(0.0004,0.0008),xaxt='n',yaxt='n',cex=1.5)
#2
testkast=read.csv("secrun/pareto_test200.csv") #read #red
pbest=cbind(testkast$X.1,testkast$X.2)
pop=48
nondkast=apply(pbest,1,function(x) testdom(x,pop,pbest))
pareto_kast=pbest[nondkast,]
pareto_log=pareto_kast
pareto_log[,1]=log10(pareto_log[,1])
plot(testkast$X.1[testkast$X.1<999],testkast$X.2[testkast$X.1<999],xlab='RMSE',ylab='Nodes',pch=3,lwd=2,col='darkorange',ylim=c(1000,1300),xlim=c(0.0004,0.0008), main="300 generations")
par(new=T)
plot(pareto_kast,xlab='',ylab='',pch=3,lwd=2,col='darkred',ylim=c(1000,1300),xlim=c(0.0004,0.0008),xaxt='n',yaxt='n',cex=1.5)
#3
testkast=read.csv("fourthrun/pareto_test50.csv") #read #red
pbest=cbind(testkast$X.1,testkast$X.2)
pop=48
nondkast=apply(pbest,1,function(x) testdom(x,pop,pbest))
pareto_kast=pbest[nondkast,]
pareto_log=pareto_kast
pareto_log[,1]=log10(pareto_log[,1])
plot(testkast$X.1[testkast$X.1<999],testkast$X.2[testkast$X.1<999],xlab='RMSE',ylab='Nodes',pch=3,lwd=2,col='darkorange',ylim=c(1000,1300),xlim=c(0.0004,0.0008), main="400 generations")
par(new=T)
plot(pareto_kast,xlab='',ylab='',pch=3,lwd=2,col='darkred',ylim=c(1000,1300),xlim=c(0.0004,0.0008),xaxt='n',yaxt='n',cex=1.5)
#4
testkast=read.csv("elevrun/pareto_test25.csv") #read #red
pbest=cbind(testkast$X.1,testkast$X.2)
pop=48
nondkast=apply(pbest,1,function(x) testdom(x,pop,pbest))
pareto_kast=pbest[nondkast,]
pareto_log=pareto_kast
pareto_log[,1]=log10(pareto_log[,1])
plot(testkast$X.1[testkast$X.1<999],testkast$X.2[testkast$X.1<999],xlab='RMSE',ylab='Nodes',pch=3,lwd=2,col='darkorange',ylim=c(1000,1300),xlim=c(0.0004,0.0008), main="600 generations")
par(new=T)
plot(pareto_kast,xlab='',ylab='',pch=3,lwd=2,col='darkred',ylim=c(1000,1300),xlim=c(0.0004,0.0008),xaxt='n',yaxt='n',cex=1.5)
#5
testkast=read.csv("nineteenrun/pareto_test15.csv") #read #red
pbest=cbind(testkast$X.1,testkast$X.2)
pop=48
nondkast=apply(pbest,1,function(x) testdom(x,pop,pbest))
pareto_kast=pbest[nondkast,]
pareto_log=pareto_kast
pareto_log[,1]=log10(pareto_log[,1])
plot(testkast$X.1[testkast$X.1<999],testkast$X.2[testkast$X.1<999],xlab='RMSE',ylab='Nodes',pch=3,lwd=2,col='darkorange',ylim=c(1000,1300),xlim=c(0.0004,0.0008), main="800 generations")
par(new=T)
plot(pareto_kast,xlab='',ylab='',pch=3,lwd=2,col='darkred',ylim=c(1000,1300),xlim=c(0.0004,0.0008),xaxt='n',yaxt='n',cex=1.5)
#6
testkast=read.csv("pareto_test50.csv") #read #red
pbest=cbind(testkast$X.1,testkast$X.2)
pop=48
nondkast=apply(pbest,1,function(x) testdom(x,pop,pbest))
pareto_kast=pbest[nondkast,]
pareto_log=pareto_kast
pareto_log[,1]=log10(pareto_log[,1])
plot(testkast$X.1[testkast$X.1<999],testkast$X.2[testkast$X.1<999],xlab='RMSE',ylab='Nodes',pch=3,lwd=2,col='darkorange',ylim=c(1000,1300),xlim=c(0.0004,0.0008), main="1010 generations")
par(new=T)
plot(pareto_kast,xlab='',ylab='',pch=3,lwd=2,col='darkred',ylim=c(1000,1300),xlim=c(0.0004,0.0008),xaxt='n',yaxt='n',cex=1.5)

dev.off()