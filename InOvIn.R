data=read.table(file="/data/InOvIn3.csv",header = F,sep=",")
r=length(data[,1])
l=length(data[1,])
d_lst=sample(nrow(data))
r_data=data[d_lst,]
nc=0
adj=matrix(nrow=1,ncol=1)
adj_mtrx=matrix(nrow=3,ncol=3)

degree=matrix(nrow=2,ncol=2)
degree[is.na(degree)] <- 0

for(i in 1:r)
{
  
  
  
  
  if(i==1)
  {
    
    adj_mtrx[(i+1),i]=adj_mtrx[i,(i+1)]=r_data[i,i]
    adj_mtrx[(i+2),i]=adj_mtrx[i,(i+2)]=r_data[i,(i+1)]
    adj_mtrx[(i+1),(i+2)]=1
    adj_mtrx[(i+2),(i+1)]=1
    Ad_dim=dim(adj_mtrx)
   
    degree[i,i]=r_data[i,i]
    degree[(i+1),i]=r_data[i,(i+1)]
    degree[i,(i+1)]=degree[i,(i+1)]+1
    degree[(i+1),(i+1)]=degree[(i+1),(i+1)]+1
    dm=dim(degree)
    gm=matrix(nrow=2,ncol=2)
    gm[is.na(gm)] <- 0
    degree=rbind(degree,gm)
    
    p1=dim(adj_mtrx)
    co=matrix(nrow=p1[1],ncol=2)
    adj_mtrx=cbind(adj_mtrx,co)
    
   
    p1=dim(adj_mtrx)
    co1=matrix(nrow=2,ncol=p1[2])
    adj_mtrx=rbind(adj_mtrx,co1)
    
    
  }
  else{
    
    
    
    
    if(!is.na(match(r_data[i,1],adj_mtrx[,1]))&&!is.na(match(r_data[i,2],adj_mtrx[,1]))){
      
      pos1=match(r_data[i,1],adj_mtrx[,1])
      pos2=match(r_data[i,2],adj_mtrx[,1])
      adj_mtrx[pos1,pos2]=1
      adj_mtrx[pos2,pos1]=1
      
      degree[(pos1-1),2]=degree[(pos1-1),2]+1
      degree[(pos2-1),2]=degree[(pos2-1),2]+1
      
     
    }
    else if(!is.na(match(r_data[i,1],adj_mtrx[,1]))||!is.na(match(r_data[i,2],adj_mtrx[,1]))){
      
      if(!is.na(match(r_data[i,1],adj_mtrx[,1]))){
        
        pos=match(r_data[i,1],adj_mtrx[,1])
        npos=pos
        adj_mtrx[(Ad_dim[1]+1),1]=adj_mtrx[1,(Ad_dim[1]+1)]=r_data[i,2]
        
        adj_mtrx[pos,Ad_dim[1]+1]=1
        adj_mtrx[Ad_dim[1]+1,pos]=1
        
        degree[(pos-1),2]=degree[(pos-1),2]+1
        degree[(dm[1]+1),1]=r_data[i,2]
        degree[(dm[1]+1),2]=degree[(dm[1]+1),2]+1
        
        
        
        
      }else{
        
        pos=match(r_data[i,2],adj_mtrx[,1])
        npos=pos
        adj_mtrx[(Ad_dim[1]+1),1]=adj_mtrx[1,(Ad_dim[1]+1)]=r_data[i,1]
        
        adj_mtrx[pos,Ad_dim[1]+1]=1
        adj_mtrx[Ad_dim[1]+1,pos]=1
        
        degree[(pos-1),2]=degree[(pos-1),2]+1
        degree[(dm[1]+1),1]=r_data[i,1]
        degree[(dm[1]+1),2]=degree[(dm[1]+1),2]+1
        
        
        
      }
      
      Ad_dim=dim(adj_mtrx)-1
      dm=dim(degree)-1
      gm=matrix(nrow=1,ncol=2)
      gm[is.na(gm)] <- 0
      degree=rbind(degree,gm)
      
      
      p1=dim(adj_mtrx)
      co=matrix(nrow=p1[1],ncol=1)
      adj_mtrx=cbind(adj_mtrx,co)
      
      
      
    
      p1=dim(adj_mtrx)
      co1=matrix(nrow=1,ncol=p1[2])
      adj_mtrx=rbind(adj_mtrx,co1)
      
    }
    else{
      
      
      adj_mtrx[(Ad_dim[1]+1),1]=adj_mtrx[1,(Ad_dim[1]+1)]=r_data[i,1]
      adj_mtrx[(Ad_dim[1]+2),1]=adj_mtrx[1,(Ad_dim[1]+2)]=r_data[i,2]
      adj_mtrx[(Ad_dim[1]+1),(Ad_dim[1]+2)]=1
      adj_mtrx[(Ad_dim[1]+2),(Ad_dim[1]+1)]=1
      
      degree[(dm[1]+1),1]=r_data[i,1]
      degree[(dm[1]+2),1]=r_data[i,2]
      degree[(dm[1]+1),2]=degree[(dm[1]+1),2]+1
      degree[(dm[1]+2),2]=degree[(dm[1]+2),2]+1
      
      
      
      
      dm=dim(degree)
      gm=matrix(nrow=2,ncol=2)
      gm[is.na(gm)] <- 0
      degree=rbind(degree,gm)
      
      Ad_dim=dim(adj_mtrx)
    
      p1=dim(adj_mtrx)
      co=matrix(nrow=p1[1],ncol=2)
      adj_mtrx=cbind(adj_mtrx,co)
     
      
      
      
      p1=dim(adj_mtrx)
      co1=matrix(nrow=2,ncol=p1[2])
      adj_mtrx=rbind(adj_mtrx,co1)
      
    } 
  }
}


nc=0
b=0
a=0
r=length(degree[,1])-2
#l=length(r_data[1,])
L1=matrix(nrow=2,ncol=1)
L1[is.na(L1)] <- 0
L2=matrix(nrow=1,ncol=1)
L2[is.na(L2)] <- 0
for(i in 1:r){
  pos=match(degree[i,1],adj_mtrx[,1])
  adj_dta=adj_mtrx[pos,-1]
  dt=which(adj_dta==1)+1
  dl=adj_mtrx[1,c(dt)]
  d_mtrx=matrix(nrow=1,ncol=length(dl))
  d_mtrx[is.na(d_mtrx)] <- 0
  for(u in 1:length(dl)){
    p_l1=match(dl[u],degree[,1])
    d_mtrx[1,u]=degree[p_l1,2]
  }
  for(j in 1:length(dl)){
    
    # dl[which.max(d_mtrx)]
    pos1=match(dl[which.max(d_mtrx)],degree[,1])
    if(j==1){
      max=max(d_mtrx)
      avg=round(mean(d_mtrx))
    }else{
      max=max(d_mtrx)
    }
    
    if((degree[pos1,2])>avg){
      avg1=round(mean(d_mtrx))
      d_mtrx[1,(which.max(d_mtrx))]=0
      # miu=(degree[(pos-1),2])/((degree[(pos-1),2])+((degree[pos1,2])-(degree[(pos-1),2])))
      kl=is.element(dl,L1)
      # if(sum(kl==T)!=0){
      #  max=max+1
      #}
      #miu=(((max)-(avg1))+(sum(kl==T)))/(length(dl))
      miu=((max)-(avg1))/(length(dl))
      if(miu>0.5){
        ol=dim(L1)
        if(ol[2]==1){
          #ps2=match((degree[pos1,1]),L1[1,])
          L1[2,1]=L1[2,1]+1
        }else{
          if(((degree[pos1,1]) %in% (L1[1,]))){
            ps2=match((degree[pos1,1]),L1[1,])
            L1[2,ps2]=L1[2,ps2]+1
          }else{
            a=a+1
            L1[2,a]=L1[2,a]+1
          }
        }
        if(!((degree[pos1,1]) %in% (L1[1,]))){
          nc=nc+1
          a=nc
          l1=matrix(nrow=2,ncol=1)
          l1[is.na(l1)] <- 0
          L1[1,nc]=degree[pos1,1]   ###pos1
          # L1[2,nc]=L1[2,nc]+1
          L1=cbind(L1,l1)
          # L1[1,nc]=degree[pos1,1]
        }
        # else{
        #   pos2=match((degree[pos1,1]),L1[1,])
        #  L1[2,pos2]=L1[2,pos2]+1
        #}
      }
    }else{
      j=length(dl)
    }
    
    
    
    
  }
}


nc=0
max1=0
#ouT=0
#iN=0
L1=L1[-2,]
L1=t(L1)
fg=dim(L1)
L1_fin=matrix(nrow=2,ncol=fg[2])
gh=dim(L1)
#nl=matrix(nrow=2,ncol=gh[2])
#L1_fin=rbind(L1_fin,nl)
iN=0
ouT=0
for(i in 1:((length(L1[1,]))-1))
{
  pos=match(L1[1,i],adj_mtrx[,1])
  adj_dta=adj_mtrx[pos,-1]
  dt=which(adj_dta==1)+1
  dl=adj_mtrx[1,c(dt)]
  d_mtrx=matrix(nrow=1,ncol=length(dl))
  d_mtrx[is.na(d_mtrx)] <- 0
  for(u in 1:length(dl)){
    p_l1=match(dl[u],degree[,1])
    d_mtrx[1,u]=degree[p_l1,2]
  }
  for(j in 1:length(dl)){
    
    # dl[which.max(d_mtrx)]
    pos1=match(dl[which.max(d_mtrx)],degree[,1])  ###find the max
    if(j==1){
      max=max(d_mtrx)
      avg=round(mean(d_mtrx))
      max1=max+1
    }else{
      max=max(d_mtrx)
    }
    
    ###if((degree[pos1,2])>=avg){
    avg1=round(mean(d_mtrx))
    d_mtrx[1,(which.max(d_mtrx))]=0
    #miu=(max-avg1)/(length(dl))
    miu=(max-avg1)/(length(dl))  
    if(miu>=0.5 && max1>max){  #miu<=0.5  
     
      ouT=ouT+1
      max1=max
    }else{
      iN=iN+1
      max1=max
    }
    
  }
  pos12=match(L1[1,i],degree[,1])
  deg=degree[pos12,2]
  if((iN/deg)>=0.5){
    L1_fin[1,i]=L1[1,i]
    # L1_fin[3,i]=1
    L1_fin[2,i]=1
  }else{
    L1_fin[1,i]=L1[1,i]
    #L1_fin[3,i]=0
    L1_fin[2,i]=0
  }  
  
  
  
  iN=0
  ouT=0
}


L1_intrin=matrix(nrow=1,ncol=gh[2])
for(i in 1:((length(L1[1,]))-1)){
  if(L1_fin[2,i]==1){  ################# && or || check
    
    L1_intrin[1,i]=L1_fin[1,i]
  }
  
  
}

###########################################################
L1_int=L1_intrin[!is.na(L1_intrin)]
L1_intrin=t(L1_intrin[,colMeans(is.na(L1_intrin)) == 0])


Final=0
L3=0
if(length(L1_intrin)>=3){
  #tl=0
  #L_or=L1
  L3=matrix(nrow=1,ncol=1)
  L3[is.na(L3)] <- 0
  nc=0
  for(i in 1:(length(L1_intrin[1,])))
  {
    pos=match(L1_intrin[1,i],adj_mtrx[,1])
    adj_dta=adj_mtrx[pos,-1]
    dt=which(adj_dta==1)+1
    dl=adj_mtrx[1,c(dt)]
    d_mtrx=matrix(nrow=1,ncol=length(dl))
    d_mtrx[is.na(d_mtrx)] <- 0
    for(u in 1:length(dl)){
      p_l1=match(dl[u],degree[,1])
      d_mtrx[1,u]=degree[p_l1,2]
    }
    
    for(j in 1:length(dl)){
      
      # dl[which.max(d_mtrx)]
      pos1=match(dl[which.max(d_mtrx)],degree[,1])
      if(j==1){
        max=max(d_mtrx)
        avg=round(mean(d_mtrx))
      }else{
        max=max(d_mtrx)
      }
      
      if((degree[pos1,2])>avg){
        if(!((degree[pos1,1]) %in% (L1_intrin[1,]))||!((degree[pos1,1]) %in% (L3[1,]))){
          #if(!((degree[pos1,1]) %in% (L3[1,]))){  
          avg1=round(mean(d_mtrx))
          d_mtrx[1,(which.max(d_mtrx))]=0
          
          miu=((max)-(avg1))/(length(dl))
          if(miu<=0.5){   #miu<=0.5)
            nc=nc+1
            p1=matrix(nrow=1,ncol=1)
            p1[is.na(p1)] <- 0
            L3[1,nc]=degree[pos1,1]   
            # L1[2,nc]=L1[2,nc]+1
            L3=cbind(L3,p1)
            max1=max
          }else if(max1==max){    
            nc=nc+1
            p1=matrix(nrow=1,ncol=1)
            p1[is.na(p1)] <- 0
            L3[1,nc]=degree[pos1,1]   
            # L1[2,nc]=L1[2,nc]+1
            L3=cbind(L3,p1)
            max1=max
            
          }
          
        }
      }else if((degree[pos1,2])==avg){
        
        
        d_mtrx[1,(which.max(d_mtrx))]=0
        
        pos=match(degree[pos1,1],adj_mtrx[,1])
        adj_dta=adj_mtrx[pos,-1]
        dt_f=which(adj_dta==1)+1
        dl_f=adj_mtrx[1,c(dt_f)]
        d_mtrx_f=matrix(nrow=1,ncol=length(dl_f))
        d_mtrx_f[is.na(d_mtrx_f)] <- 0
        for(u in 1:length(dl_f)){
          p_l1_f=match(dl_f[u],degree[,1])
          d_mtrx_f[1,u]=degree[p_l1_f,2]
        }
        
        avg2=round(mean(d_mtrx_f)) 
        if(avg2>=avg){
          
          if(!((degree[pos1,1]) %in% (L1_intrin[1,])) ||!((degree[pos1,1]) %in% (L3[1,]))){
            #if(!((degree[pos1,1]) %in% (L3[1,]))){
            nc=nc+1
            p1=matrix(nrow=1,ncol=1)
            p1[is.na(p1)] <- 0
            L3[1,nc]=degree[pos1,1]   ###pos1
            # L1[2,nc]=L1[2,nc]+1
            L3=cbind(L3,p1)
            
          }
          
          
          
          
        }
      }else{                               
        
        d_mtrx[1,(which.max(d_mtrx))]=0
        
        pos=match(degree[pos1,1],adj_mtrx[,1])
        adj_dta=adj_mtrx[pos,-1]
        dt_f_1=which(adj_dta==1)+1
        dl_f_1=adj_mtrx[1,c(dt_f_1)]
        d_mtrx_f_1=matrix(nrow=1,ncol=length(dl_f_1))
        d_mtrx_f_1[is.na(d_mtrx_f_1)] <- 0
        for(u in 1:length(dl_f_1)){
          p_l1_f_1=match(dl_f_1[u],degree[,1])
          d_mtrx_f_1[1,u]=degree[p_l1_f_1,2]
        }
        avg3=round(mean(d_mtrx_f_1))
        Fin=c(L1_intrin,L3)
        Fin=unique(Fin[Fin>0])
        kl=is.element(dl_f_1,Fin)
        if((sum(kl==T))/(length(dl_f_1))>0.7 && avg3>=avg ){ 
          
          nc=nc+1
          p1=matrix(nrow=1,ncol=1)
          p1[is.na(p1)] <- 0
          L3[1,nc]=degree[pos1,1]   
          # L1[2,nc]=L1[2,nc]+1
          L3=cbind(L3,p1)
          
        }
      }
      
      
    }
    
  }
  Final=c(L1_intrin,L3)
  Final=unique(Final[Final>0])
}
