#------------------------------------------------------------------------------#
#
### RUN SERVER OPERATIONS FOR CANOPY BACK-END
#
#------------------------------------------------------------------------------#

#------------------------------------------------------------------------------#
### Calculate Benchmarks -------------------------------------------------------
#------------------------------------------------------------------------------#

### Calculate multiple benchmarks of the objective:
xs_ij <- data.table(xs_ij, key="i,j")

### Uniform distribution of staff
V_ij$s.unif <- R / J
bench.unif <- Obj()

### Distribution of staff in rough (i.e. rounded-off) proportion to neighborhood poverty
w_j <- xs_ij[, sum(w), by = j]$V1
w <- sum(w_j)
w_j <- w_j / w
V_ij$s.pov <- w_j * R
bench.pov <- Obj()

### Distribution of staff in rough (i.e. rounded-off) proportion to # of youth in given radius
n_j <- xs_ij[, length(w), by = j]
n <- sum(n_j)
n_j <- n_j / n
V_ij$s.pop <- n_j * R
bench.pop <- Obj()

### Distribution of staff based on simple simulated annealing objective -- without demand side eq
# XXX Need to think about what this is: i.e. what we could conceivably do, if we did not have
#     the resources to do a proper demand-side investigation.
# Distribution of staff based on simple simulated annealing objective -- with demand side eq
bench.can <- Obj()

# Frame the differences in equivalent # of high-poverty youth who are now expected to be successfully
#   targeted, at no cost, based simply on having better resources (data, demand side, algorithm) for planning

#------------------------------------------------------------------------------#
### Run CANOPY -----------------------------------------------------------------
#------------------------------------------------------------------------------#

Out <- canopy(obj         = Obj,
              alloc       = r_j,
              v_ij        = V_ij,
              lower       = vLowerBound,
              upper       = vUpperBound,
              proposal    = GenProposal,
              transmat    = c2c.inv,
              nudge       = updateV,
              temperature = Temp,
              iterations  = 3000, # nIter  ... 3e5
              checkpoint  =  500) # ceiling(iterations/10)
save(Out, file = "./data/out/Simulated Annealing Output.Rda")
