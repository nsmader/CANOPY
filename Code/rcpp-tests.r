# See Hadley Wickham's page for examples:
#   http://adv-r.had.co.nz/Rcpp.html
#   In particular, the 
# Also see this C++ reference: http://www.cplusplus.com/

library("Rcpp")
library("rbenchmark")

#### Modify example of vector output to 
  cppFunction('NumericVector vecMult(double x, NumericVector ys) {
    int n = ys.size();
    NumericVector out(n);
  
    for(int i = 0; i < n; ++i) {
      out[i] = ys[i] * x;
    }
    return out;
  }')
  
  y <- rnorm(1e7)
  m <- exp(0.5)
  print("##########")
  system.time(y*m)
  system.time(vecMult(m, y))
  
  benchmark(y*m, vecMult(m, y))
  
  x <- system.time(vecMult(m, y))


#### 


