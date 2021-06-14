makePed <- function (pedigree, ID=1, Sire=2, Dam=3, rmsingle = FALSE, verbose = TRUE) {
  
  anim = unique(as.character(pedigree[,ID]))
  Sires = unique(as.character(pedigree[,Sire]))
  Dams = unique(as.character(pedigree[,Dam]))
  
#check for duplicated IDs
  if(anyDuplicated(pedigree[,ID]))
    {
    message("Duplicated IDs were  detected and removed.\n")
    ord   <- order(!is.na(pedigree[,2])+!is.na(pedigree[,3]))
    pedigree <- pedigree[ord,]
    message("This includes: \n")
    print(head(pedigree[duplicated(pedigree[,1]),]))      
    pedigree <- pedigree[!duplicated(pedigree[,1]),]
  }
  rownames(pedigree)<-pedigree[,1]
  
  Sireclean = Sires[Sires!="0"]
  Damclean = Dams[Dams!="0"]
  wrongsex = Sireclean %in% Damclean
  
  if(TRUE %in% wrongsex) {
    warning (" looks like some Sires appear as Dams, assumed selfing")
  }
  
  # remove singleton animals (no parents or progeny in the pedigree)
  if(rmsingle == TRUE){
    singleton.vec <- pedigree[,1][pedigree[,2] == 0 & pedigree[,3]== 0 & !(pedigree[,1] %in% c(pedigree[,2], pedigree[,3]))]
    
    if(length(singleton.vec) > 0) {
      pedigree = pedigree[-(pedigree[,1] %in% singleton.vec),]
      if (verbose == TRUE) message("A total of", length(singleton.vec), "found and removed. \n")
    }
  }
  
  #Run check for individuals in Sire/Dam that are not in ID and add them as founders
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
    message("individuals appearing as Sire/Dam but not as individuals were added as founders. \n")
  }
  
  # wraps up pedigree stats and output information
  message(paste0('Total number of animals counted in this pedigree = ', length(anim),sep=''),'\n')
  message(paste0('Total number of Sires counted in this pedigree = ', length(Sireclean),sep=''),'\n')
  message(paste0('Total number of Dams counted in this pedigree = ', length(Damclean),sep=''),'\n')
  message("New individuals added = ", length(addFounders),'\n')
  message('Pedigree is now sorted and ordered! \n')
  if (addFounders == TRUE) {
    return(newPed)
  }
  else return(pedigree)
}

Ped <- structure(list(ID = 1:7, Sire = c(0L, 0L, 1L, 1L, 3L, 1L, 5L), Dam = c(0L, 0L, 0L, 2L, 4L, 4L, 6L)), class = "data.frame", row.names = c(NA,  -7L))

Ped = makePed(Ped)