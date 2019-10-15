fun_static=function(z,w,x,y){
  
  
  data[i,1]=x
  data[i,2]=y
  adj_mtrx=z
  degree=w
  
  
  if(data[i,1]!=1){
    pos=(match(data[i,1],adj_mtrx[,1]))
    neib_pos=which(adj_mtrx[pos,]==1)
    neib_mem1=matrix(nrow=2,ncol=length(neib_pos))
    for(k in 1:length(neib_pos)){
      
      neib_mem1[1,k]=adj_mtrx[1,neib_pos[k]]
      
      deg_pos=which(degree[,1]==adj_mtrx[1,neib_pos[k]])
      neib_mem1[2,k]=degree[deg_pos,2]
      
    }
    
  }else{
    pos=(match(data[i,1],adj_mtrx[,1]))
    neib_pos=which(adj_mtrx[pos,]==1)
    neib_pos=neib_pos[-1]
    neib_mem1=matrix(nrow=2,ncol=length(neib_pos))
    for(k in 1:length(neib_pos)){
      
      neib_mem1[1,k]=adj_mtrx[1,neib_pos[k]]
      
      deg_pos=which(degree[,1]==adj_mtrx[1,neib_pos[k]])
      neib_mem1[2,k]=degree[deg_pos,2]
      
    }
    
  }
  

  if(data[i,2]!=1){
    pos=(match(data[i,2],adj_mtrx[,1]))
    neib_pos=which(adj_mtrx[pos,]==1)
    neib_mem2=matrix(nrow=2,ncol=length(neib_pos))
    for(k in 1:length(neib_pos)){
      
      neib_mem2[1,k]=adj_mtrx[1,neib_pos[k]]
      
      deg_pos=which(degree[,1]==adj_mtrx[1,neib_pos[k]])
      neib_mem2[2,k]=degree[deg_pos,2]
      
    }
    
  }else{
    pos=(match(data[i,2],adj_mtrx[,1]))
    neib_pos=which(adj_mtrx[pos,]==1)
    neib_pos=neib_pos[-1]
    neib_mem2=matrix(nrow=2,ncol=length(neib_pos))
    for(k in 1:length(neib_pos)){
      
      neib_mem2[1,k]=adj_mtrx[1,neib_pos[k]]
      
      deg_pos=which(degree[,1]==adj_mtrx[1,neib_pos[k]])
      neib_mem2[2,k]=degree[deg_pos,2]
      
    }
    
  }
  
 
  sum_1=sum(neib_mem1[2,])
  sum_2=sum(neib_mem2[2,])
  
  common=match(neib_mem1[1,],neib_mem2[1,])  
  common=which(common!="NA")
  #tr=!is.na(match(common,cluster))  
  Com_len=length(common)
  dim_1=dim(neib_mem1)
  dim_2=dim(neib_mem2)
  len_negh=min(dim_1[2],dim_2[2]) 
  srt1=sort(neib_mem1[2,],decreasing =T)
  srt2=sort(neib_mem2[2,],decreasing = T)
  dif_fin=0
  
  for(y in 1:len_negh){
    
    diff=abs(srt2[y]-srt1[y])
    dif_fin=abs(dif_fin+diff)
    
  }
  if(dif_fin>Com_len){
    Ex_sim=abs((dif_fin+Com_len)/(sum_1+sum_2))
  }else{
    Ex_sim=1
    
  }
  
  ########################################
  dif_in_1=0
  
  
  deg_pos1=(match(data[i,1],degree[,1]))
  deg_pos2=(match(data[i,2],degree[,1]))
  
  
  for(g in 1:dim_1[2]){
    
    diff=abs(degree[deg_pos1,2]-neib_mem1[2,g])
    dif_in_1=dif_in_1+diff
    
  }
  
  
  ###################################
  dif_in_2=0
  
  for(g in 1:dim_2[2]){
    
    diff=abs(degree[deg_pos2,2]-neib_mem2[2,g])
    dif_in_2=dif_in_2+diff
    
  }
  deg_1=degree[deg_pos1,2]
  deg_2=degree[deg_pos2,2]
  
  if(deg_1==deg_2){
    Minn=c(dif_in_1,dif_in_2)
    min=which.min(Minn)
    
    if(min==1){
      
      Thrsh=abs((dif_in_1)/(dif_in_2))
    }
    else{
      
      Thrsh=abs((dif_in_2)/(dif_in_1))
      
    }
    
    
    
    
  }else{
    
    deg_12=c(deg_1,deg_2)
    min=which.min(deg_12)
    
    if(min==1){
      
      Thrsh=abs((dif_in_1)/(dif_in_2))
    }
    else{
      
      Thrsh=abs((dif_in_2)/(dif_in_1))
      
    }
    
  }
  ##########################################
  
  
  #}
  result=c(Ex_sim,Thrsh)
  
  return(result)  
}
