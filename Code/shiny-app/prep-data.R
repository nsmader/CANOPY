#------------------------------------------------------------------------------#
#
# PREP DATA USED FOR THE CANOPY METHOD
#
#------------------------------------------------------------------------------#

### Load Data and Prepare Meta Values ------------------------------------------

### Load youth data 
load("./data/youth-to-court-data.Rda")
x_ij <- data.table(y2c)
NC <- nrow(x_ij) # N is the number of youth, C(i) is the choice set available to individual
i.u <- unique(x_ij$i)
j.u <- unique(x_ij$j)
N <- length(i.u)
J <- length(j.u)

## Load court-to-court data
load("./data/court-to-court-distances.Rda")
c2c <- data.table(c2c)
colnames(c2c) <- as.character(1:J)
rownames(c2c) <- as.character(1:J)

### Reduce the size of objects to practically test timing
iSub <- sample(i.u, 50000)
jSub <- sample(j.u, 50)
x_ij <- x_ij[i %in% iSub]
x_ij <- x_ij[j %in% jSub]

### Set up base xb
x_ij[, xb := -1.5 + (-1.0)*d + (-2.0)*d*cr]

c2c <- c2c[jSub, c(jSub), with = FALSE]

### Reset the sizes of objects
NC <- nrow(x_ij)
i.u <- unique(x_ij$i)
j.u <- unique(x_ij$j)
N <- length(i.u)
J <- length(j.u)

### Prepare inverse distances between courts
c2c.inv <- 1/c2c
diag(c2c.inv) <- 0 # Otherwise, this is infinite
J == nrow(c2c.inv) # Check that the number of courts in court-to-court data is the same as from y2c data

