library(corrplot)
Amat <- Matrix::readMM("/gpfs1/home/victor.oribamise/polA.txt")
correlateSparse <- function(x){
    n <- nrow(x)
    cMeans <- colMeans(x)
    covmat <- (as.matrix(crossprod(x)) - n*tcrossprod(cMeans))/(n-1)
    sdvec <- sqrt(diag(covmat)) 
    cormat <- covmat/tcrossprod(sdvec)
    list(cov=covmat,cor=cormat)
}
e <- correlateSparse(Amat)
k <- corrplot(e$cor, type = "upper", method="square")

write.table(e$cor, "/gpfs1/home/victor.oribamise/POlcor.txt", quote=FALSE, row.names=FALSE)
