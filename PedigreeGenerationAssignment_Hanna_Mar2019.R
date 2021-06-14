GENAssign = function(Ped, ID=1, Sire=2, Dam=3,st = TRUE, header=TRUE){
  Ped = cbind(Ped,matrix(0,nrow=dim(Ped)[1],ncol=1))
  colnames(Ped)=array(c("ID", "Sire", "Dam", "GEN"))
  Ped[is.na(Ped)] <- 0
  Ped[which(Ped[,2]==0 & Ped[,3]==0),4]=1
  fpl=array(c(unique(Ped[which(Ped[,2]>0),2]),unique(Ped[which(Ped[,3]>0),3])))
  i=1
  while(dim(fpl)>0){
    pl = array(Ped[which(Ped[,4]==i),1])
    fpl=array(subset(fpl,!(fpl%in%pl)))
    Ped[which((Ped[,2]%in%pl & Ped[,3]%in%pl) | (Ped[,2]%in%pl & !(Ped[,3]%in%pl) & !(Ped[,3]%in%fpl)) | (Ped[,3]%in%pl & !(Ped[,2]%in%pl) & !(Ped[,2]%in%fpl))),4]=i+1
    i=i+1
  }
  if (st == TRUE) {
    Ped = Ped[order(Ped[,4],Ped[,2],Ped[,3]),]
  }
  
  return(Ped)
}

Ped = structure(list(ID = 1:7, Sire = c(0L, 0L, 1L, 1L, 3L, 1L, 5L), 
    Dam = c(0L, 0L, 0L, 2L, 4L, 4L, 6L)), class = "data.frame", row.names = c(NA, 
-7L))


PedGen = GENAssign(Ped)