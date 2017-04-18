# pso no shuffling no bad neighbourhoood
library(compiler)
source('mesheval.R')
mo_PSO_R=function(pop,dim,xmin,xmax,gen,maxeval,start_shuffle_prob=0.995){
  
  # embedded objective functions from cec 2013 real-parameter opti 
  fit_func<-function(ln_id){
    # change for mesh
     result=eval_fun(ln_id)
    return(result) # this should be maxeval rows if combined opti is used
  }
  
  reshuffle_prob=start_shuffle_prob
  if(length(xmax)!=dim){
    stop('xmax: wrong number of boundaries defined. Dimension and length(xmax) differ')
  }
  if(length(xmin)!=dim){
    stop('xmin: wrong number of boundaries defined. Dimension and length(xmin) differ')
  }
  ###################
  #### Initialisation
  ###################
  
  vmax=0.3*(xmax-xmin)
  swarm=matrix(t(runif(pop*dim,xmin,xmax)),nrow=pop,byrow=T) 
  vel=matrix(rep(0,dim*pop),nrow=pop,byrow=T) 
  pbest_loc=read.csv('pbestin25.csv',header=T)
  pbest_loc=as.matrix(pbest_loc[,2:8])
  swarm=pbest_loc
  xmax_mat=matrix(rep(xmax,pop),nrow=pop,byrow=T)
  xmin_mat=matrix(rep(xmin,pop),nrow=pop,byrow=T)
  
  # parallel evaluation
  index=sort(rep(1:ceiling((pop/maxeval)),maxeval))
  index=index[1:pop]
  for(i in 1:ceiling(pop/maxeval)){
    ln_id=(length(index[index==i]))
    pars_in=cbind(rep('p',ln_id),matrix(swarm[index==i,],nrow=ln_id))
    write(t(pars_in),'pars.in',append = F,ncol=dim+1)
    if(i>1){
      result=rbind(result,fit_func(ln_id))
    }else{
      result=fit_func(ln_id)
    }
  }
  pbest=result
  ranks_ob1=rank(result[,1],ties.method = "random")
  ranks_ob2=rank(result[,2],ties.method = "random")
  ## Neighbourhoods
  # 1. each particle only has two neighbours to exchange information, which are neighhbours based on objective function 1
  # 2. The particle is going to move towards the best particle in that small neighbourhood
  # based on objective function 2
  # 3. later pbest is only updated when non-dominated solution presetn, e.g. solution has improved for both objective functions
  # 4. can I do this vectorized as well?
  nhood=function(i){
    ind=ranks[i]
    if(ind==1){
      n1=which(ranks==(ind+1))
      n2=which(ranks==(ind+1))
    }else{
      if(ind==pop){
        n1=which(ranks==(ind-1))
        n2=which(ranks==(ind-1))
      }else{
        n1=which(ranks==(ind-1))
        n2=which(ranks==(ind+1))
      }
    }
    nhood=c(i,n1,n2)
    nmin=which.min(ranks2[nhood])
    nbests=nhood[nmin]
    return(nbests)
  }
  
  mode1=T
  switch=0
  if(mode1){
    ranks=ranks_ob1
    ranks2=ranks_ob2
  }else{
    ranks=ranks_ob2
    ranks2=ranks_ob1
  }
  pops=matrix(1:pop,ncol=1)
  nbests=apply(pops,1,nhood)
  nbests_loc=swarm[nbests,]

  # # bad neighbourhoods
  # bad_hood=matrix(nrow=ceiling(length(ranks_ob1)/2),ncol=dim*2)
  # centr=swarm[which(ranks_ob1>floor(pop/2)),]
  # sph=seq(from=1,to=0.3/(ceiling(pop/2)),length=ceiling(pop/2))
  # bad_ranks=ranks_ob1[which(ranks_ob1>floor(pop/2))]
  # centr=centr[rev(sort(bad_ranks,index.return=T)$ix),]
  # hood=as.matrix(sph)%*%t(as.matrix(vmax))
  # bad_hood=cbind(centr-hood,centr+hood)
  ##############
  # Update
  ##############
  k=1 # generation index
  results=list() # to be returned in the end
  while(k<=gen){
    mode1=!mode1
    wmax=(0.9-0.2)*(gen-k)/gen+0.2 # based on Suganthan, Roshida and yoshida et al. #0.9
    #wmin=0.2
    vmax=(0.3*(xmax-xmin)-(xmax-xmin)/20)*(gen-k)/gen+(xmax-xmin)/20#changed from 0.5 to 0.3 and from min 10 to 20 (run 18). changed 0.1 to 0.5  
    #particle specific vmax, low when gbest (improve exploitation) and high when ranked badly (go on exploration)
    vmax_part=matrix(rep(vmax,pop),nrow=pop,byrow=T)/(pop-ranks+1)
    w=wmax
    c2=0.5+(2.5-0.5)*k/gen ## increasing attraction to global best
    c1=0.1+(1.5-0.1)*(gen-k)/gen # decreasing attraction to personal best
    vel=w*vel+c1*runif(pop)*(pbest_loc-swarm)+c2*runif(pop)*(nbests_loc-swarm)
    
    too_fast=which(vel>vmax_part)
    vel[too_fast]=vmax_part[too_fast]
    too_fast=which(vel<(-vmax_part))
    vel[too_fast]=-vmax_part[too_fast]
    
    swarm=swarm+vel
    # reflection back into the space when 'hitting' the boundary
    bound_max=which(swarm > xmax_mat)
    bound_min=which(swarm < xmin_mat)
    swarm[bound_max] = xmax_mat[bound_max]-(swarm[bound_max]-xmax_mat[bound_max])
    swarm[bound_min] = xmin_mat[bound_min]-(swarm[bound_min]-xmin_mat[bound_min])
    
    #testing if in bad neighboruhood
    # in_bad_hood=rep(FALSE,pop)
    # for(n in 1:(round(length(ranks)/2))){
    #   bool_vec=apply(swarm,1,function(x) sum(x>=bad_hood[n,1:dim]&x<=bad_hood[n,(dim+1):(2*dim)])==dim)
    #   in_bad_hood[bool_vec]=TRUE
    # }
    # ln_bn=length(in_bad_hood[in_bad_hood])
    # rndm_gb=matrix(runif(dim*ln_bn),nrow=ln_bn,byrow=T)
    # swarm[in_bad_hood,]=swarm[in_bad_hood,]+c2*rndm_gb*(nbests_loc[in_bad_hood,]-swarm[in_bad_hood,])
    # 
    # parallel evaluation
    for(i in 1:ceiling(pop/maxeval)){
      ln_id=(length(index[index==i]))
      pars_in=cbind(rep('p',ln_id),matrix(swarm[index==i,],nrow=ln_id))
      write(t(pars_in),'pars.in',append = F,ncol=dim+1)
      if(i>1){
        result=rbind(result,fit_func(ln_id))
      }else{
        result=fit_func(ln_id)
      }
    }
    ranks_ob1=rank(result[,1],ties.method = "random")
    ranks_ob2=rank(result[,2],ties.method = "random")
    
    if(mode1){
      ranks=ranks_ob1
      ranks2=ranks_ob2
    }else{
      ranks=ranks_ob2
      ranks2=ranks_ob1
    }
    
    reshuffle_prob=reshuffle_prob*start_shuffle_prob
    a=runif(1)
    # reinitilize worst half if prob reached
    if(a>reshuffle_prob){
      worst=length(ranks[which(ranks>floor(pop/2))])
      swarm[which(ranks>floor(pop/2)),]=matrix(t(runif(worst*dim,xmin,xmax)),nrow=worst,byrow=T) 
      reshuffle_prob=start_shuffle_prob
      for(i in 1:ceiling(pop/maxeval)){
        ln_id=(length(index[index==i]))
        pars_in=cbind(rep('p',ln_id),matrix(swarm[index==i,],nrow=ln_id))
        write(t(pars_in),'pars.in',append = F,ncol=dim+1)
        if(i>1){
          result=rbind(result,fit_func(ln_id))
        }else{
          result=fit_func(ln_id)
        }
      }
      ranks_ob1=rank(result[,1],ties.method = "random")
      ranks_ob2=rank(result[,2],ties.method = "random")
      
      if(mode1){
        ranks=ranks_ob1
        ranks2=ranks_ob2
      }else{
        ranks=ranks_ob2
        ranks2=ranks_ob1
      }
    }
    # 
    # # new bad neigbourhood
    # bad_hood=matrix(nrow=ceiling(length(ranks)/2),ncol=dim*2)
    # centr=swarm[which(ranks>floor(pop/2)),]
    # sph=seq(from=1,to=0.2/(ceiling(pop/2)),length=ceiling(pop/2))
    # bad_ranks=ranks[which(ranks>floor(pop/2))]
    # centr=centr[rev(sort(bad_ranks,index.return=T)$ix),]
    # hood=as.matrix(sph)%*%t(as.matrix(vmax))
    # bad_hood=cbind(centr-hood,centr+hood)
    
    #new pbest, only if both are better
    newpbest=apply(result<=pbest,1,all)
    pbest[newpbest,]=result[newpbest,]
    pbest_loc[newpbest,]=swarm[newpbest,]
    nbests=apply(pops,1,nhood)
    nbests_loc=swarm[nbests,]
    results[[k]]=cbind(pbest_loc,pbest)
    pdf(paste(k,'mesh_opti_pareto.pdf',sep=""))
    plot(pbest[pbest[,1]<999,1],pbest[pbest[,1]<999,2],xlab='obj1',ylab='obj2',main=paste('pareto front mesh optimization:',k))
    dev.off()
    write.csv(results[[k]],paste('pareto_test',k,'.csv',sep=""))
    k=k+1  
  }
  # pareto optimal front
  testdom=function(testobj){
    test=matrix(rep(testobj,pop),ncol=2,byrow=T)
    bool=!any(apply(test>pbest,1,all))
  }
  nond=apply(pbest,1,testdom)
  pareto=cbind(pbest_loc[nond,],pbest[nond,])
  #print(pareto)
  results[[k]]=pareto
  return(results)
}

  mo_PSO=cmpfun(mo_PSO_R)
