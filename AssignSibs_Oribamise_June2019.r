AssignSibs = function(Ped,colnames = c("ID","Sire","Dam"),st = TRUE,savefiles = TRUE){
  library(data.table)
  Ped <- data.table(Ped)
  sibDT = Ped[!is.na(Sire) & !is.na(Dam), 
              CJ(ID = ID, fullsibs = ID)[ID != fullsibs]
              , by=.(Sire, Dam)]
  hsibDT = melt(Ped, id.vars  = "ID")[!is.na(value), 
                                      CJ(ID = ID, hsid = ID)[ID != hsid]
                                      , by=.(ptype = variable, pid = value)][!sibDT, on=.(ID, hsid = fullsibs)]
  Ped[sibDT[, .(fullsibs = toString(fullsibs)), by=ID], on=.(ID), fullsibs := i.fullsibs, by=.EACHI ]
  Ped[hsibDT[, .(hsibs = toString(hsid)), by=ID], on=.(ID), hsibs := i.hsibs, by=.EACHI ]
  Ped[
    rbind(sibDT[, .(ID, oid = fullsibs)], hsibDT[, .(ID, oid = hsid)])[, 
                                                                       .(fams = toString(oid))
                                                                       , by=.(ID)], 
    on = .(ID),
    fams := i.fams
    , by = .EACHI]
  #if (savefiles == TRUE) {
    
 #   write.table(Ped,paste0(""),quote=FALSE, row.names=FALSE, col.names = TRUE, sep= "\t")
  #}
  message(paste0('Total number of half-sibs in this pedigree = ', length(table(Ped[,"hsibs"])), sep=""))
  message(paste0('Total number of full-sibs in this pedigree = ', length(table(Ped[,"fullsibs"])), sep=""))
  message(paste0('Total number of DFC in this pedigree = ', length(table(Ped[,"fams"])), sep=""))
  return(Ped)
}

Ped = structure(list(ID = c("1", "2", "3", "4", "6", "5", "7"), Sire = c(0L, 
0L, "1", "1", "1", "3", "5"), Dam = c(0L, 0L, 0L, "2", "4", "4", 
"6")), class = c("data.table", "data.frame"), row.0Lmes = c(0L, 
-7L))
AssSibs = AssignSibs(Ped)


