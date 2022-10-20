#include <Rcpp.h>
#include <cmath>
#include <numeric>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;
using namespace Rcpp; 

/* this code computes the nestedness of a given incident matrix M 
# according to the definition given in 
# Fortuna, M.A., et al.: Coevolutionary dynamics shape the structure of bacteria‐phage infection networks. Evolution 1001-1011 (2019). 
# DOI 10.1111/evo.13731
 */

// [[Rcpp::export]]
double nestednessCpp(const NumericMatrix & M) {
  IntegerMatrix B(M.nrow(), M.ncol());	
  
  int i, j, nr, nc;
  nr = M.nrow(); 
  nc = M.ncol(); 
  
  //binarize the matrix 
  for (int i = 0; i < nc*nr; i++) {
    if(M[i]>0){
      B[i]=1;
    } else {
      B[i]=0;
    }
  }
  
  // nestedness of rows
  double nestedness_rows = 0;
  for(int i = 0; i < (nr-1); i++) {
    j = i + 1;
    while (j < nr) {
      IntegerVector v_i = B( i , _ );
      IntegerVector v_j = B( j , _ );
      
      int k_i = accumulate(v_i.begin(), v_i.end(), 0);
      int k_j = accumulate(v_j.begin(), v_j.end(), 0);
      
      int shared = inner_product(v_i.begin(), v_i.end(), v_j.begin(), 0);
      
      // handle disconnected nodes 
      if (!((k_i == 0) || (k_j==0))){
        int min_shared = min(k_i, k_j); // min of the degrees
        nestedness_rows = nestedness_rows + (1.0*shared/min_shared);
      }
      j++;
    } // end while iterator
  } //   end for loop 
  
  
  // nestedness of columns
  double nestedness_cols = 0;
  for(int i = 0; i < (nc-1); i++) {
    j = i + 1;
    while (j < nc) {
      
      IntegerVector v_i = B( _ , i );
      IntegerVector v_j = B( _ , j );
      
      int k_i = accumulate(v_i.begin(), v_i.end(), 0);
      int k_j = accumulate(v_j.begin(), v_j.end(), 0);
      
      int shared = inner_product(v_i.begin(), v_i.end(), v_j.begin(), 0); 
      
      // handle disconnected nodes 
      if (!((k_i == 0) || (k_j==0))){
        int min_shared = min(k_i, k_j); // min of the degrees
        nestedness_cols = nestedness_cols + (1.0*shared/min_shared);
      }
      j++; 
    } // end while iterator
  } //   end for loop 
  
  //  Compute nestedness of the network
  double nestedness_val = (nestedness_rows + nestedness_cols) / ((nr * (nr - 1) / 2) + (nc * (nc - 1) / 2));
  
  return nestedness_val; 
}