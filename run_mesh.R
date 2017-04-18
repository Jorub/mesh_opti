# start mesh opti
source('mo_PSO.R')
source('mesheval.R')

rn_int=sample(1e+8,1)
set.seed(rn_int)
write(rn_int,"rand_seed.out")

maxeval=8 # number of parallel oeprataions 
system(paste('./opti_setup.sh', maxeval))

xmin=c(5,5,15,6,10,10,20)
xmax=c(10,10,20,12,20,15,50)

mesh_opti=mo_PSO(48,7,xmin=xmin,xmax=xmax,50,maxeval,1000)
pareto=mesh_opti[[length(mesh_opti)]]
write.csv(pareto,'pareto.csv')
write.table(pareto,'pareto.txt')
print(pareto)
pdf('mesh_opti_pareto.pdf')
plot(pareto[,8],pareto[,9],xlab='obj1',ylab='obj2',main='pareto front mesh optimization')
dev.off()
