
ref1=read.table("ref1.out", quote="\"")
ref1=ref1[,3]
ref2=read.table("ref2.out", quote="\"")
ref2=ref2[,3]
ref3=read.table("ref3.out", quote="\"")
ref3=ref3[,3]

eval_fun= function(ln_id){
  system2("./mesh_opti.sh")
  result=matrix(ncol=2,nrow=ln_id)
  for(i in 1:ln_id){
    told=1
    tnew=0
    while(tnew!=told){
      told=tnew
      obs1=read.table(paste(i,"/out/obspt_RE_matrix-1.out",sep=''), quote="\"",comment.char="#", sep="")
      timeobs1=obs1[,1]
      tnew=length(timeobs1)
    }
    obs1=obs1[,3]
    
    obs2=read.table(paste(i,"/out/obspt_RE_matrix-2.out",sep=''), quote="\"")
    obs2=obs2[,3]
    
    obs3=read.table(paste(i,"/out/obspt_RE_matrix-3.out",sep=''), quote="\"")
    obs3=obs3[,3]
    test_length=c(length(obs1),length(obs2),length(obs3))
    if(any(test_length<201)){
      result[i,1]=999
    }else{
      res1=RMSE(ref1,obs1)
      print(res1)
      res2=RMSE(ref2,obs2)
      print(res2)
      res3=RMSE(ref3,obs3)
      print(res3)
      result[i,1]=1/3*sum(res1,res2,res3)
    }
    nodes=read.table(paste(i,"/nodes.out",sep=''), quote="\"")
    result[i,2]=nodes$V1
    #colnames(result)=c('obj1','obj2')
  }
  #rownames(result)=rep((''),ln_id) 
  return(result)#
}

RMSE=function(sim, real){
  result=sqrt(sum((sim-real)^2)/length(real))
  return(result)
}
