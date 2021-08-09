Rcpp::sourceCpp('makeD.cpp')
library(Matrix)
Ped <- structure(list(ID = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 10L, 
                             11L, 12L, 13L, 14L, 18L, 15L, 16L, 17L, 20L, 19L), Sire = c(NA, 
                                                                                         NA, NA, NA, NA, NA, NA, NA, NA, NA, 1L, 1L, 4L, 6L, 4L, 10L, 
                                                                                         12L, 13L, 13L, 14L), Dam = c(NA, NA, NA, NA, NA, NA, NA, NA, 
                                                                                                                      NA, NA, 2L, 3L, 2L, 5L, 11L, 11L, 5L, 3L, 7L, 2L), GEN = c(1L, 
                                                                                                                                                                                 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 
                                                                                                                                                                                 3L, 3L, 3L)), class = "data.frame", row.names = c(NA, -20L))


A=createD(Ped$ID, Ped$Sire, Ped$Dam)

