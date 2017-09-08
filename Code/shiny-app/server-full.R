#------------------------------------------------------------------------------#
#
### RUN SERVER OPERATIONS FOR CANOPY BACK-END
#
#------------------------------------------------------------------------------#

shinyServer(function(input, output) {

#------------------------------------------------------------------------------#
### Update Data According to User Input ----------------------------------------
#------------------------------------------------------------------------------#

### Initialize uniform allocation to all sites --------------------------------#

r_j <- reactive(data.table(j = j.u, r = floor(input$R/J)))
r_j()[1:(input$R %% J), r := r + 1] # Add the remainder 

### Receiv User Weights -------------------------------------------------------#
w <- 
  reactive({
    data.table(pov = c("n0_50FPL", "n50_99FPL", "100_199FPL", "n200_.FPL"),
               w = c(input$w0to50, input$w50to99, input$w100to199, input$w200to.))
  })
w_i <- 
  reactive({
    merge(x = x_ij,
          y = w(),
          by = "pov",
          all.x = TRUE)[
            order(i), .(i, w)]
  })

#------------------------------------------------------------------------------#
### Calculate Benchmarks -------------------------------------------------------
#------------------------------------------------------------------------------#

### Uniform distribution of staff ---------------------------------------------#

ru_j <- reactive(data.table(j = j.u, r = floor(input$R/J)))
ru_j()[1:(input$R %% J), r := r + 1] # Add the remainder 

Vu <- UpdateVR(x = x_ij, r = ru_j())
bench.unif <- Obj(Vu, w_i)

### Distribution proporational to neighborhood poverty ------------------------#
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

### Determine weight for youth in objective
wMap <- c("n200_.FPL"   = 1,
          "n100_199FPL" = 2,
          "n50_99FPL"   = 3,
          "n0_50FPL"    = 4)
x_ij[, w := wMap[as.character(pov)]]
# /!\ Update the weights to be based on user input


### Set Function To Be Minimized -----------------------------------------------

### The objective is a weighted sum of the probability that each youth participates in any park
Obj <- function(V_ij){
  sumV <- V_ij[, sum(eV), by = i]
  vSumExbs <- as.vector(sumV$V1)
  p_i <- as.vector(vSumExbs / (1 + vSumExbs))
  #all(table(w_i[,"i"] == p_i$i)) # This checks that all youth are in the same order
  t(w_i[,2]) %*% p_i
}

### Set a function to adjust the valuations according to the proposed change to the state
# jFrom <- sample(j.u, 1); jTo <- sample(j.u, 1)
upVal <- exp(+1*0.5); downVal <- exp(-1*0.5)
updateV <- function(V_ij, jFrom, jTo){
  V_ij[j == jFrom, eV := eV*downVal]
  V_ij[j == jTo,   eV := eV*upVal]
  return(V_ij)
}

### Starting resources are equal number of staff as sites
r_j <- data.table(j = j.u, r = 1)
R <- r_j[, sum(r), drop = T]

### Set lower and upper bounds
vLowerBound <- as.vector(rep(0, J))
vUpperBound <- as.vector(rep(J, J)) # This is effectively unconstrained, except at zero below


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

})