### CLUSTERING WITH LARGE FILE ###

rm(list=ls())

library("parallel", lib.loc="~/R/R-3.2.1/library")

workerFunc <- function(n) { return(n^2) }
values <- 1:100

## Number of workers (R processes) to use:
numWorkers <- 4
## Set up the 'cluster'
#cl <- makeCluster(numWorkers, type = "PSOCK") or
cl <- makeCluster(getOption("cl.cores", 4))
## Parallel calculation (parLapply):
res <- parLapply(cl, values, workerFunc)
## Shut down cluster
stopCluster(cl)
print(unlist(res))


install.packages("snow", lib="C:/Users/.../R/R-3.2.1/library")
library(snow)

data <- datamatrix2

parallel.function <- function(i) {
  ;kmeans( data, centers=4, nstart=i )
}
cl <- makeCluster(numWorkers, type = "PSOCK")
#cl <- makeCluster( mpi.universe.size(), type="MPI" )
clusterExport(cl, c('data'))

results <- parLapply( cl, c(25,25,25,25), fun=parallel.function )

temp.vector <- sapply( results, function(result) { result$tot.withinss } )
result <- results[[which.min(temp.vector)]]
print(result)

stopCluster(cl)
mpi.exit()
