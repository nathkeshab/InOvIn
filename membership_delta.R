  delta=function(data){
  data=read.table(file="D:/ImPleMenTatioN/R/Graph visualization/Data/karate11.csv",header = F,sep=",")
  len=length(data[,1])
  source("D:/ImPleMenTatioN/R/adj_static.r")
  adj_mtrx=fun(data)
  source("D:/ImPleMenTatioN/R/deg_static.r")
  degree=fun(data)
  tk=dim(degree)
  bn=matrix(nrow=tk[1],ncol=1)
  bn[is.na(bn)] <- 0
  degree=cbind(degree,bn)
  cluster=matrix(nrow=1,ncol=2)
  #cluster[is.na(cluster)] <- 0
  for(i in 1:len){
    
    
    
    if(i==1){
      
      cluster[i,1]=data[i,1]
      cluster[i,2]=data[i,2]
      
      clu=matrix(nrow=1,ncol=2)
      cluster=cbind(cluster,clu)
      ###########################excluded neighbors###########
      pos1=match(data[i,1],degree[,1])
      pos2=match(data[i,2],degree[,1])
      
      
      degree[pos1,3]=degree[pos2,3]=0
      
      ########################################################
      
    }
    else{
      
      if(!is.na(match(data[i,1],cluster)) && !is.na(match(data[i,2],cluster))){
        
        #if(!is.na(match(data[i,1],cluster))){
          
          cls_len=length(cluster[,1])
          for(j in 1:cls_len){
            
              lo=cluster[j,]
              lo=lo[!is.na(lo)]
              
              if(!is.na(match(data[i,1],lo))){
                
                po1=j
                
              }
              if(!is.na(match(data[i,2],lo))){
                
                po2=j
                
                  }
               }
          if(po1!=po2)  
          {
            pos1=match(data[i,1],degree[,1])
            pos2=match(data[i,2],degree[,1])
            
            if(degree[pos1,2]!=degree[pos2,2])  ###degree difference
            {
              v_g=which.max(c(degree[pos1,2],degree[pos2,2])) ##hub node
              
              ######################community size############
              cs1=cluster[po1,]
              cs1=cs1[!is.na(cs1)]
              
              cs2=cluster[po2,]
              cs2=cs2[!is.na(cs2)]
              
              #########################################
              v_s=which.max(c(length(cs1),length(cs2)))  ####size
              
              if(v_g=v_s){
                  if(v_g==1){
                    ml=which(is.na(cluster[po1,]))
                    
                    if(length(ml)!=0){
                    cluster[po1,ml[1]]=data[i,2]
                    }
                    else{
                      
                      cdm=dim(cluster)
                      nl=matrix(nrow=cmd[1],ncol=1)
                      cluster=cbind(cluster,nl)
                      
                      ml=which(is.na(cluster[po1,]))
                      cluster[po1,ml[1]]=data[i,2]
                      
                    }
                  }else{
                    ml=which(is.na(cluster[po2,]))
                    if(length(ml)!=0){
                    cluster[po2,ml[1]]=data[i,1] 
                    }else{
                      
                      cdm=dim(cluster)
                      nl=matrix(nrow=cmd[1],ncol=1)
                      cluster=cbind(cluster,nl)
                      
                      ml=which(is.na(cluster[po2,]))
                      cluster[po2,ml[1]]=data[i,1]
                      
                    }
                  }
                
              }else{
                
                if(degree[pos1,2]>degree[pos2,2]){
                  ###############stability##############
                
                  d_mtrx_f=matrix(nrow=2,ncol=length(cs1))
                  d_mtrx_f[is.na(d_mtrx_f)] <- 0
                  for(u in 1:length(cs1)){
                    p_l1_f=match(cs1[u],degree[,1])
                    d_mtrx_f[1,u]=degree[p_l1_f,2]
                  }
                  hi=0
                  for(u in 1:length(cs1)){
                    po_deg=match(cs1[u],degree[,1])
                    if(degree[po_deg,2]>=mean(d_mtrx_f, na.rm=TRUE)){
                      hi=hi++
                    po_cs=match(cs1[u],adj_mtrx[,1])
                    adj_dta=adj_mtrx[po_cs,-1]
                    dt_f=which(adj_dta==1)+1
                    dl_f=adj_mtrx[1,c(dt_f)]
                    
                    mth=match(dl_f,cs1)
                    op=which(!is.na(mth))
                    po_deg=match(cs1[u],degree[,1])
                    
                    share=(length(op))/(degree[po_deg,2])
                    d_mtrx_f[2,u]=share
                    }
                    
                  }
                  stv=sum(d_mtrx_f[2,]>0.5, na.rm=TRUE)
                  if((stv/hi)<0.5){   ###if not stable data will go## if stable also data wont be added bca size is small
                    
                    ml=which(is.na(cluster[po2,]))
                    
                    if(length(ml)!=0){
                      cluster[po2,ml[1]]=data[i,1]
                    }
                    else{
                      
                      cdm=dim(cluster)
                      nl=matrix(nrow=cmd[1],ncol=1)
                      cluster=cbind(cluster,nl)
                      
                      ml=which(is.na(cluster[po2,]))
                      cluster[po2,ml[1]]=data[i,1]
                        }
                    
                  }
                  
                  hi=0
                  
                  
                  }
                  
                  
                  
                  ######################################
                  
                  
                }
                
                
                ###############membership with own community#########
                miu=
                
                
                
                #####################################################
                ######################membership with the guest node community######
                
                
                
                #########################################################
                
              }
            }else{
              
              
              
              
            }
            
            
            
        
            
          }
          
            
          #}
          
        } 
        
        
        
        pos1=match(r_data[i,1],adj_mtrx[,1])
        pos2=match(r_data[i,2],adj_mtrx[,1])
        adj_mtrx[pos1,pos2]=1
        adj_mtrx[pos2,pos1]=1
      
      
      
      }
      
    }
    pos=match(data[i,1],adj_mtrx[,1])
    
    deg_host=degree[(pos-1),2]
    
    adj_dta=adj_mtrx[pos,-1]
    dt=which(adj_dta==1)+1
    dl=adj_mtrx[1,c(dt)]
    
    d_mtrx=matrix(nrow=1,ncol=length(dl))
    d_mtrx[is.na(d_mtrx)] <- 0
    for(u in 1:length(dl)){
      p_l1=match(dl[u],degree[,1])
      d_mtrx[1,u]=degree[p_l1,2]
    }
    
    
    
    
  }
  
  
  
  
  
  
  
  
  
#}