# Parallel processing code

# 2024-01-27

library(parallel)

parallel::detectCores()

n.cores <- parallel::detectCores() - 1

#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
  )

#check cluster definition (optional)
print(my.cluster)

#register it to be used by %dopar%
doParallel::registerDoParallel(cl = my.cluster)

#check if it is registered (optional)
foreach::getDoParRegistered()

#how many workers are available? (optional)
foreach::getDoParWorkers()

# run code
# foreach(..., .inorder=TRUE, .packages="foreach") %dopar% {
          
#          }

# Don't forget to stop cluster
# This did not work for me
parallel::stopCluster(cl = my.cluster)  # summary.connection(connection) : invalid connection

showConnections()  # when not parallel processing: "description class mode text isopen can read can write"

registerDoSEQ()

# Try this
unregister <- function() {
  env <- foreach:::.foreachGlobals
  rm(list=ls(name=env), pos=env)
}



# References
# https://www.blasbenito.com/post/02_parallelizing_loops_with_r/