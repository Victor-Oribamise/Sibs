AssignSibs = function(Ped,colnames = c("ID","Sire","Dam"),st = TRUE,savefiles = TRUE){
  setDT(Ped)[,msib:=.(list(ID)), by = "Dam"][
    ,msibs := mapply(setdiff, msib, ID)][
      ,fsib  := .(list(ID)), by = "Sire"][
        ,fsibs := mapply(setdiff, fsib, ID)][
          ,Sibs  := mapply(union, msibs, fsibs)][
            ,c("msib","msibs", "fsib", "fsibs") := NULL]
  #if (savefiles == TRUE) {
   # fwrite(Ped,paste("C:\\Users\\User1\\Desktop\\truegen.txt"),quote=FALSE,row.names=FALSE, col.names = TRUE)
  #}
  return(Ped)
}


#example
Ped = structure(list(ID = c("1", "2", "3", "4", "6", "5", "7"), Sire = c(0L,
0L, "1", "1", "1", "3", "5"), Dam = c(0L, 0L, 0L, "2", "4", "4",
"6")), class = c("data.table", "data.frame"), row.0Lmes = c(0L,
-7L))
AssSibs = AssignSibs(ed)
