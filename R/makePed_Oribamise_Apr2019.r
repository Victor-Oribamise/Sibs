makePed <- function (pedigree, ID=1, Sire=2, Dam=3, rmsingle = FALSE, verbose = TRUE) {
  
  anim = unique(as.character(pedigree[,ID]))
  Sires = unique(as.character(pedigree[,Sire]))
  Dams = unique(as.character(pedigree[,Dam]))
  
  #check for duplicated IDs
  if (length(anim) != length(pedigree[,ID]))
  {
    dupID = pedigree[,1][duplicated(pedigree[,1])]
    cat(" Duplicated IDs detected ! \n")
    cat(" -> ", (dupID[!duplicated(dupID)]), "\n")
    cat(" Duplicated IDs position are represented by -1! \n")
    pedigree[,1][duplicated(pedigree[,1])] = -1
    return(pedigree[,1])
  }
  
  Sireclean = Sires[Sires!="0"]
  Damclean = Dams[Dams!="0"]
  wrongsex = Sireclean %in% Damclean
  
  if(TRUE %in% wrongsex) {
    stop (" Sire = Dam  ")
  }
  
  # remove singleton animals (no parents or progeny in the pedigree)
  if(rmsingle == TRUE){
    singleton.vec <- pedigree[,1][pedigree[,2] == 0 & pedigree[,3]== 0 & !(pedigree[,1] %in% c(pedigree[,2], pedigree[,3]))]
    
    if(length(singleton.vec) > 0) {
      pedigree = pedigree[-(pedigree[,1] %in% singleton.vec),]
      if (verbose == TRUE) cat("A total of", length(singleton.vec), "found and removed. \n")
    }
  }
  
  # check for individuals in Sire/Dam that are not in ID and add them as founders
  if(any(!pedigree[,2] %in% pedigree[,1]))
	  Sirenew <- data.frame(ID = pedigree[which(!pedigree[,2] %in% pedigree[,1]), 2],
                          Sire = 0, Dam = 0)
  if(any(!pedigree[,3] %in% pedigree[,1]))
    Damnew <- data.frame(ID = pedigree[which(!pedigree[,3] %in% pedigree[,1]), 3],
                         Sire = 0, Dam = 0)
  Sirenew <- unique(subset(Sirenew, ID != 0))
  Damnew <- unique(subset(Damnew, ID != 0))
  
  newPed <- as.data.frame(rbind(Sirenew, Damnew, pedigree))
  
  addFounders = TRUE
  
  if(length(newPed) > length(pedigree)) {
    addFounders == TRUE
    cat("individuals appearing as Sire/Dam but not as individuals were added as founders. \n")
  }
  
  # wraps up pedigree stats and output information
  cat(paste0('Total number of animals counted in this pedigree = ', length(anim),sep=''),'\n')
  cat(paste0('Total number of Sires counted in this pedigree = ', length(Sireclean),sep=''),'\n')
  cat(paste0('Total number of Dams counted in this pedigree = ', length(Damclean),sep=''),'\n')
  
  #cat('Pedigree is sorted and ordered! \n')
  if (addFounders == TRUE) return(newPed)
  else return(pedigree)
}

Ped <- structure(list(ID = 1:7, Sire = c(0L, 0L, 1L, 1L, 3L, 1L, 5L), Dam = c(0L, 0L, 0L, 2L, 4L, 4L, 6L)), class = "data.frame", row.names = c(NA,  -7L))

Ped = makePed(Ped)