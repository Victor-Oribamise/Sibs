#define ARMA_64BIT_WORD
#include <RcppArmadillo.h>
// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;
using namespace arma;

// [[Rcpp::export]]
sp_mat createA(IntegerVector ID, IntegerVector Sire, IntegerVector Dam){//, IntegerVector gen){
  int n; 
  int s; int d;
  
  double tmp;
  
  sp_mat A(Sire.size(), Sire.size());
  
  A.diag().ones();
  n = max(ID); 
  
  for(int t = 0; t < n; t++){
    s = std::max(Sire[t], Dam[t]); 
    d = std::min(Sire[t], Dam[t]);
    
    //A(t,t) = 2-pow(0.91, gen[t] - 1);
    
    if ((s>0) & (d>0) ) { 
        A(t,t) +=  0.5*A(Sire[t]-1,Dam[t]-1);
      //A(t,t) +=  pow(0.5, gen[t])*A(Sire[t]-1,Dam[t]-1);
      for(int j = 0; j < t; j++){
        
        tmp = 0.5 * (A(j, Sire[t]-1) + A(j, Dam[t]-1));
        if (tmp > 0){
          A(t,j) = tmp;
          A(j,t) = tmp;
        }
      }
    } else if ((s>0) & (d==0)) {
      
      for(int j = 0; j < t; j++){
        tmp = 0.5 * A(j, s-1);
        if (tmp > 0){
          A(t,j) = tmp;
          A(j,t) = tmp;
        }
      }
    }
  }
  
  return(A);
}
