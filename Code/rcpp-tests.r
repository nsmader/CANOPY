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


#### Trying alternative code for the key operation

  # Current R-based approach
    Obj <- function(V_ij, jFrom, jTo){ # upVal, downVal # ... the Deltas are always -1 and 1 in this implementation
      
      from_ij <- V_ij$j == jFrom; to_ij <- V_ij$j == jTo
      
      #system.time({
        V_ij$V[from_ij] <- V_ij$V[from_ij]*downVal
        V_ij$V[to_ij  ] <- V_ij$V[to_ij  ]*upVal
        sumV <- V_ij[, sum(V), by=i]
        vSumExbs <- as.vector(sumV$V1)
        p_i <- as.vector(vSumExbs / (1 + vSumExbs))
        #table(w_i[,1] == sumV$i) # Checking that youth ids are in the same order.
        t(w_i[,2]) %*% p_i
      #})
      
    }
    benchmark(Obj(V_ij, sample(j.u, 1), sample(j.u, 1)))
  # Takes, on average, 0.04 seconds. To get 100,000 steps requires 4,000 seconds.
  #   That's a little over an hour.
    #system.time(for (i in 1:1) Obj(V_ij, sample(j.u, 1), sample(j.u, 1)))

  # R-based alternative to current Obj() code
    # Approach of fully subsetting records, to modify just those affected by the change
        # XXX Note: updates to V_ij aren't saved for the next iteration.
        # Thus, we'd need to join the new V values back to the original data set, which likely makes this
        #   close to equivalent with the approach above.
        # Then again, because we will not accept all proposals, we do not want to modify the data
        #   when we call Obj(). This, then, should be a faster way to go.
        system.time(
          system.time(for (i in 1:100) iUpd <- V_ij$i[from_ij | to_ij])
            # This seems to take up a lot of clock time: 0.03. Half of it is calculating "from_ij | to_ij"
          system.time(VUpd <- V_ij[V_ij$i %in% iUpd,]) # This one too: 0.03 -- see if we can speed up subsetting with data.table
          VUpd$V2 <- VUpd$V
          
          VUpd$V2[VUpd$j == jFrom] <- VUpd$V2[VUpd$j == jFrom]*downVal
          VUpd$V2[VUpd$j == jTo  ] <- VUpd$V2[VUpd$j == jTo  ]*upVal
          
          VSums <- VUpd[, lapply(.SD, sum), by="i", .SDcols=c("V", "V2")]
          pDiff_i <- (VSums$V2 / (1 + VSums$V2)) - (VSums$V / (1 + VSums$V))
          #table(w_i[,1] == sumV$i) # Checking that youth ids are in the same order.
          score <- score + t(w_i[w_i[,1] %in% iUpd,2]) %*% pDiff_i
        })


