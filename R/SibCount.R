#############################################
### Code to create function "SibCount"    ###
### Lauren Hanna                          ###
### Last Update: Jan. 2020                ###
#############################################

FullSibCount = function(Ped){
  FSCount = cbind(unique(Ped[,c("Sire","Dam")]),data.frame(NuFullSib=0))
  FSCount = FSCount[!rowSums(is.na(FSCount[,c("Sire","Dam")])),]
  for(i in 1:dim(FSCount)[1]){
    FSCount[i,"NuFullSib"]=nrow(subset(Ped,(Ped[,"Sire"]==FSCount[i,"Sire"] & Ped[,"Dam"]==FSCount[i,"Dam"])))
  }
  FullSibs = FSCount[which(FSCount[,"NuFullSib"]>=2),]
  if(dim(FullSibs)[1]>0){
    FullSibStats = matrix(c(nrow(FullSibs),round(mean(FullSibs[,"NuFullSib"]),3),round(sd(FullSibs[,"NuFullSib"]),3),min(FullSibs[,"NuFullSib"]),max(FullSibs[,"NuFullSib"])),ncol=5,dimnames=list("FullSib",c("Count","Average","SD","Min","Max")))
  }else{FullSibStats = matrix(0,nrow=1,ncol=5,dimnames=list("FullSib",c("Count","Average","SD","Min","Max")))}
  return(list(FullSibs,FullSibStats))
}

PHSibCount = function(Ped){
  PHSCount = cbind(Sire=unique(Ped[,"Sire"]),data.frame(NuPHalfSib=0))
  PHSCount = PHSCount[!is.na(PHSCount[,"Sire"]),]
  for(i in 1:dim(PHSCount)[1]){
    PHSCount[i,"NuPHalfSib"]=nrow(Ped[which(Ped[,"Sire"]==PHSCount[i,"Sire"]),])
  }
  PHSCount = PHSCount[which(PHSCount[,"NuPHalfSib"]>=2),]
  if(dim(PHSCount)[1]>0){
    PHSStats = matrix(c(nrow(PHSCount),round(mean(PHSCount[,"NuPHalfSib"]),3),round(sd(PHSCount[,"NuPHalfSib"]),3),min(PHSCount[,"NuPHalfSib"]),max(PHSCount[,"NuPHalfSib"])),ncol=5,dimnames=list("PaternalHalfSib",c("Count","Average","SD","Min","Max")))
  }else{PHSStats = matrix(0,nrow=1,ncol=5,dimnames=list("PaternalHalfSib",c("Count","Average","SD","Min","Max")))}
  return(list(PHSCount,PHSStats))
}

MHSibCount = function(Ped){
  MHSCount = cbind(Dam=unique(Ped[,"Dam"]),data.frame(NuMHalfSib=0))
  MHSCount = MHSCount[!is.na(MHSCount[,"Dam"]),]
  for(i in 1:dim(MHSCount)[1]){
    MHSCount[i,"NuMHalfSib"]=nrow(Ped[which(Ped[,"Dam"]==MHSCount[i,"Dam"]),])
  }
  MHSCount = MHSCount[which(MHSCount[,"NuMHalfSib"]>=2),]
  if(dim(MHSCount)[1]>0){
  MHSStats = matrix(c(nrow(MHSCount),round(mean(MHSCount[,"NuMHalfSib"]),3),round(sd(MHSCount[,"NuMHalfSib"]),3),min(MHSCount[,"NuMHalfSib"]),max(MHSCount[,"NuMHalfSib"])),ncol=5,dimnames=list("MaternalHalfSib",c("Count","Average","SD","Min","Max")))
  }else{MHSStats = matrix(0,nrow=1,ncol=5,dimnames=list("MaternalHalfSib",c("Count","Average","SD","Min","Max")))}
  return(list(MHSCount,MHSStats))
}

SibCount = function(Ped,savefiles=TRUE,destfile){
  if(savefiles == TRUE & missing(destfile)){
    stop("ERROR: No path was specified for saving files.")
  }
  Ped[which(Ped[,"Sire"]==0),"Sire"]=NA
  Ped[which(Ped[,"Dam"]==0),"Dam"]=NA
  FullSibs = FullSibCount(Ped)
  PHS = PHSibCount(Ped)
  MHS = MHSibCount(Ped)
  Stats = rbind(FullSibs[[2]],rbind(PHS[[2]],MHS[[2]]))
  if(savefiles == TRUE){
    write.table(FullSibs[[1]],paste(getwd(),"/FullSibs_",Sys.Date(),".txt",sep=""),quote = FALSE,row.names=FALSE)
    write.table(PHS[[1]],paste(getwd(),"/PatHalfSibs_",Sys.Date(),".txt",sep=""),quote = FALSE,row.names=FALSE)
    write.table(MHS[[1]],paste(getwd(),"/MatHalfSibs_",Sys.Date(),".txt",sep=""),quote = FALSE,row.names=FALSE)
    write.table(Stats,paste(getwd(),"/SibStatistics_",Sys.Date(),".txt",sep=""),quote = FALSE)
  }
  return(Stats)
}
