source("syntheticDataFuncs.R")

set.seed(20)
betaMat <- getBetas()
yieldMat <- getYields(betaMat)

plot(tenors,yieldMat[nrow(yieldMat),])

