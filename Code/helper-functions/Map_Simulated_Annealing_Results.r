####################################################
#
# MAP OUTPUT FROM SAMPLE SIMULATED ANNEALING METHOD
#
#   The objective function is to allocate seats to 
#   odd-numbered community areas.
#
####################################################


#install.packages("maptools")
#install.packages("RColorBrewer")
#install.packages("classInt")
library("maptools"); library("RColorBrewer"); library("classInt")
"%&%" <- function(...){paste(..., sep="")}

MyDir <- "C:/Users/nmader/Documents/Econ/Chapin Hall Projects/Head Start Reallocation/"
load(file = paste(MyDir, "Prepped Data/Simulated Annealing Output", sep=""))

#--------------------#
# Set Run Parameters #
#--------------------#

  ReadGeoData  <- TRUE
  GenGraphs    <- TRUE

#-----------------------------------#
# # # Read Data and Set Options # # #
#-----------------------------------#

if (TRUE == ReadGeoData) {
  shpCCA <- readShapePoly(MyDir %&% "Raw Data/CommAreas.shp")
  names(shpCCA)

  dfHist <- data.frame(Out$CheckPoints$StateHistory)
    dfHist <- cbind(1:nrow(dfHist), dfHist)
      rownames(dfHist) <- "CCA" %&% 1:nrow(dfHist)
  
    dfHist[,ncol(dfHist)] <- Out$"Best State" # Replace the final column with the best solution that was found
      colnames(dfHist) <- c("CcaNum", "CheckPoint_" %&% 0:(ncol(dfHist)-2))
    
    dfHist$CcaNum <- as.character(dfHist$CcaNum)
    
  Time  <- data.frame(Out$CheckPoints$RunTime)
  Steps <- data.frame(Out$CheckPoints$Step)
  
  # Pick Colors
  colors <- brewer.pal(5, "Blues")
  BreakInts <- c(seq(0, 800, length.out=5), max(Out$CheckPoints$StateHistory))

  # Merge State History Data into Shape File

    shpCCA_History <- merge(x=shpCCA, y=dfHist, by.x = "AREA_NUMBE", by.y = "CcaNum")
}

#------------------------------------#
# # # Create the Series of Plots # # #
#------------------------------------#

if (TRUE == GenGraphs) {

  # Set up Loop Across Checks

  for (chk in seq(0, ncol(dfHist)-1, by=5)) {

    vSeats <- as.double(shpCCA_History["CheckPoint_" %&% chk][[1]])
    iNumSteps <- Steps[chk+1]
    iSeconds  <- Time[chk+1]
    
    # Plot 

      #plot(shpCCA, col=colors[findInterval(vSeats, BreakInts, all.inside=TRUE)]) # , , , axes=F
      plot(shpCCA, col=colors[findInterval(vSeats[order(shpCCA_History$AREA_NUMBE)], BreakInts, all.inside=TRUE)]) # , , , axes=F
      box()
      title("Best Allocation Found After " %&% round(as.numeric(iSeconds),0) %&% "\n" 
        %&% " Seconds and " %&% prettyNum(as.integer(iNumSteps), big.mark = ",") %&% " Steps")
    
    # Label the CCAs and create legend
      LabelLoc <- getSpPPolygonsLabptSlots(shpCCA)
      text(LabelLoc, labels=shpCCA_History$AREA_NUMBE, cex=.5)
      MinX <- min(LabelLoc[,1])
      MinY <- min(LabelLoc[,2])
      MaxY <- max(LabelLoc[,2])
      legend(x=MinX-40000, y=MinY+70000, legend=leglabs(BreakInts), fill=colors, bty="n", cex = 0.5)
    
    # Text annotations
      #text(509000, 153500, "10KM", cex= 1)
      #text(534000,152000, "Boundary Data Crown Copyright Ordnance Survey 2009.", cex= 1)
      #text(556500, 166000, "% Participation", cex= 1)
    
    # Output
    
      dev.copy(png, MyDir %&% "Output/Allocation_Map_" %&% chk %&% ".png")
      dev.off()
  
  }
}
  
#----------------------------------#
# # # Generate an Animated Map # # #
#----------------------------------#

  # (adapted from http://stackoverflow.com/questions/1298100/creating-a-movie-from-a-series-of-plots-in-r)


  # Make the movie
  
  setwd(MyDir %&% "/Output/")
  unlink("Allocation_Movie.mpg")
  #system("convert -delay 5 Allocation*.png Allocation_Movie.mpg")
  #system("animate Allocation*.png > Allocation_Movie.mpg")

# # # To Do # # #

# 1. Debug map creation. Currently, the findInterval function is the problem.

# 2. Write up short abstract for this
    # - Send to Wladimir and Bob. Goal for exercise is to demonstrate (1) we have an implementation; (2) it's fast; (3) it's intuitive; (4) we haven't done so much yet that there isn't room for them to input and work with us
    # Later, send to Parv and Jen

