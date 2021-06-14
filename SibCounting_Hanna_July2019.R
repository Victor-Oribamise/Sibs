SibCount = function(Ped,coln = c("ID","Sire","Dam"),st = TRUE,savefiles = TRUE){
  Stats = matrix(0,nrow=3,ncol=5,byrow=TRUE,dimnames = list(c("FullSib","PaternalHalfSib","MaternalHalfSib"),c("Count","Average","SD","Min","Max")))

#Creating possible full sib families for counting...
  FSCount = cbind(unique(Ped[,c("Sire","Dam")]),data.frame(0))
  colnames(FSCount) = c("Sire","Dam","NuFullSib")

#Getting rid of NA in families list
  FSCount = FSCount[!rowSums(is.na(FSCount[,c("Sire","Dam")])),]

#Counting offspring per family
  for(i in 1:dim(FSCount)[1]){
    FSCount[i,"NuFullSib"]=nrow(Ped[which(Ped[,"Sire"]==FSCount[i,"Sire"] & Ped[,"Dam"]==FSCount[i,"Dam"]),])}

#Finding families with more than one offspring 
  FullSibs = FSCount[which(FSCount[,"NuFullSib"]>=2),]

#Full Sib family statistics
  Stats[1,] = c(nrow(FullSibs),round(mean(FullSibs[,"NuFullSib"]),3),round(sd(FullSibs[,"NuFullSib"]),3),min(FullSibs[,"NuFullSib"]),max(FullSibs[,"NuFullSib"]))

#################################################
###Paternal half-sib families
#Creating possible paternal half sib families for counting...
  PHSCount = cbind(unique(Ped[,"Sire"]),data.frame(0))
  colnames(PHSCount) = c("Sire","NuPHalfSib")

#Getting rid of NA in families list
  PHSCount = PHSCount[!is.na(PHSCount[,"Sire"]),]

#Counting offspring per paternal family
  for(i in 1:dim(PHSCount)[1]){
    PHSCount[i,"NuPHalfSib"]=nrow(Ped[which(Ped[,"Sire"]==PHSCount[i,"Sire"]),])}
  PHSCount = PHSCount[which(PHSCount[,"NuPHalfSib"]>=2),]
#Paternal half Sib family statistics
  Stats[2,] = c(nrow(PHSCount),round(mean(PHSCount[,"NuPHalfSib"]),3),round(sd(PHSCount[,"NuPHalfSib"]),3),min(PHSCount[,"NuPHalfSib"]),max(PHSCount[,"NuPHalfSib"]))
  #return(PHSCount)
################################################################
###Maternal half-sib families
#Creating possible paternal half sib families for counting...
  MHSCount = cbind(unique(Ped[,"Dam"]),data.frame(0))
  colnames(MHSCount) = c("Dam","NuMHalfSib")

#Getting rid of NA in families list
  MHSCount = MHSCount[!is.na(MHSCount[,"Dam"]),]

#Counting offspring per paternal family
  for(i in 1:dim(MHSCount)[1]){
    MHSCount[i,"NuMHalfSib"]=nrow(Ped[which(Ped[,"Dam"]==MHSCount[i,"Dam"]),])}
  MHSCount = MHSCount[which(MHSCount[,"NuMHalfSib"]>=2),]
  #return(MHSCount)
#Maternal half Sib family statistics
  Stats[3,] = c(nrow(MHSCount),round(mean(MHSCount[,"NuMHalfSib"]),3),round(sd(MHSCount[,"NuMHalfSib"]),3),min(MHSCount[,"NuMHalfSib"]),max(MHSCount[,"NuMHalfSib"]))
  message(paste0('Total number of maternal half-sibs in this pedigree = ', length(table(MHSCount)), sep=""))
  message(paste0('Total number of paternal half-sibs in this pedigree = ', length(table(PHSCount)), sep=""))
  message(paste0('Total number of full-sibs in this pedigree = ', length(table(FSCount)), sep=""))
  li = list(FSCount=as.data.frame(FSCount),PHSCount=as.data.frame(PHSCount), MHSCount= as.data.frame(MHSCount), Stats=as.data.frame(Stats))
  return(li)
}
