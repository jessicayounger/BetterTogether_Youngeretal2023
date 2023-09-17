##Function to plot multiple iterations in a standard format for resampling results
#Code slightly modified from post on stackoverflow user Lena:
#https://stackoverflow.com/questions/49321028/what-is-the-reason-for-bizarre-r-igraph-spinglass-result-after-several-iteration

# SPINGLASS PLOTTING FUNCTION
# the number of spins is now equal to the number of nodes, which is logical

spinComRec <- function(graphQgraph, # qgraph object
                       numberSpins = graphQgraph$graphAttributes$Graph$nNodes, 
                       numberIterations = 10, 
                       numberEstimations = 1000
                       
                       
){
  library(igraph)
  graphIgraph <- as.igraph(graphQgraph)
  
  for (yIt in 1:numberIterations){
    # Repeated estimation with stable number of spins
    #set.seed(3+yIt)
    clusterStabilitySpin <- list()
    for (i in 1:numberEstimations){
      #set.seed(3+i)
      clusterStabilitySpin[[i]] <- cluster_spinglass(graphIgraph, 
                                                     weights = E(graphIgraph)$weight,
                                                     spins = numberSpins,
                                                     start.temp = 1,
                                                     stop.temp = 0.01,
                                                     cool.fact = 0.99)  # default settings
      # we do not specify gamma but it should be 1 which = missing/non-missing equally important
      # also, they weights are taken into consideration
      # we do not use the version that takes negative edges into account as
      # indicating edges between different communities because that is not
      # logical for psychological networks
      
    }
    # how many clusters were estimated in each estimation?
    clusterStabilitySpinNumber <- c()
    for (i in 1:length(clusterStabilitySpin)){
      clusterStabilitySpinNumber[i] <- length(clusterStabilitySpin[[i]])
    }
    
    # what is the modularity estimate for each spin?
    clusterStabilityModularity <- c()
    for (i in 1:length(clusterStabilitySpin)){
      clusterStabilityModularity[i] <- clusterStabilitySpin[[i]]$modularity
    }
    
    ### Spinglass Recoding
    
    # Checking whether algorithm switches community-node assignment 
    # (within one and the same spin number)
    
    # compare each node membership with each other node membership
    testVector <- c()
    testMatrix <- matrix(ncol = graphQgraph$graphAttributes$Graph$nNodes, nrow = graphQgraph$graphAttributes$Graph$nNodes)
    testList <- list()
    for (k in length(clusterStabilitySpin):1){
      for (j in length(clusterStabilitySpin[[k]]$membership):1){
        for (i in length(clusterStabilitySpin[[k]]$membership):1){
          testVector[i] <- clusterStabilitySpin[[k]]$membership[j] == clusterStabilitySpin[[k]]$membership[i]
        }
        testMatrix[,j] <- testVector
      }
      testList[[k]] <- testMatrix
    }
    
    # testlist represents each node compared to all others (logical values 
    # indicating whether nodes belong to the same cluster)
    
    newMatrix <- list()
    for (l in 1:length(testList)){
      newMatrix[[l]] <- unique(testList[[l]], MARGIN = 2)
    }
    
    # now we took all unique logical vectors (we only have vectors that 
    # logically indicate clusters) - each column in newmatrix represents one cluster
    # newMatrix holds all unique vectors
    
    # the following substitutes each TRUE for its respective column number for each
    # list element in newMatrix and thereby recodes membership labels
    
    membershipVector <- c()
    membershipMatrix <- matrix(nrow = graphQgraph$graphAttributes$Graph$nNodes, ncol = length(clusterStabilitySpin))
    for (m in 1:length(newMatrix)){
      for (n in 1:ncol(newMatrix[[m]])){
        for (o in 1:length(which(newMatrix[[m]][,n]))){
          membershipVector[which(newMatrix[[m]][,n])[o]] <- n
        }
      }
      membershipMatrix[,m] <- membershipVector
    }
    
    
    # Loop that compares all logical matrices to all matrices
    identMatrix <- matrix(ncol = length(newMatrix), nrow = length(newMatrix)) 
    for (i in 1:length(newMatrix)){
      for (j in 1:length(newMatrix)){
        identMatrix[i,j] <- identical(newMatrix[[i]], newMatrix[[j]])
      }
    }
    
    uniqIdentMatrix <- unique(identMatrix, MARGIN = 2) 
    # extracting unique comparisons (i.e., matrices that are equal to each other 
    # (indicated by the row number)
    
    
    # See how many comparable to each other
    equalMatrixList <- list()
    for (k in 1:ncol(uniqIdentMatrix)){
      equalMatrixList[[k]] <- which(uniqIdentMatrix[,k]==TRUE)
    }
    # equalMatrixList # each list element lists the indexes of those matrices that are equal to each other
    # sapply(equalMatrixList, length)
    # which(sapply(equalMatrixList, length)==max(sapply(equalMatrixList, length))) 
    # for each of the unique memberships we get the frequency and the one with the most frequent est
    
    uniqueMembershipVectors <- matrix(nrow = nrow(membershipMatrix), ncol = length(equalMatrixList))
    for (i in 1:length(equalMatrixList)){
      uniqueMembershipVectors[,i] <- as.matrix(membershipMatrix[,equalMatrixList[[i]]])[,1]
    }
    
    # order the membership estimations according to frequency
    equalMatrixList <- equalMatrixList[order(sapply(equalMatrixList,length),decreasing=T)]
    
    # run one spinglass estimation to use as object to put in the new membershipvectors
    communitySGPlot1 <- cluster_spinglass(graphIgraph, 
                                          weights = E(graphIgraph),
                                          spins = 2)
    E(graphIgraph)$weight=ifelse(E(graphIgraph)$weight>-0.1 & E(graphIgraph)$weight<0.1,0, E(graphIgraph)$weight)
    E(graphIgraph)$lty <- ifelse(E(graphIgraph)$weight>0, 1, ifelse(E(graphIgraph)$weight==0, 0, 2))
    E(graphIgraph)$width <- abs(E(graphIgraph)$weight*15)
    # create a pdf for each iteration with a network on each page 
    # the network shows the communities, the number of communities, and the proportion
    # of all estimations in which this constellation has come up
    # the plotting starts with the estimation that has come up most frequently
    
    
    
  }
  
  list(uniqueMembershipVectors = uniqueMembershipVectors, communitySGPlot1 = communitySGPlot1, equalMatrixList = equalMatrixList, clusterStabilitySpin = clusterStabilitySpin, graphIgraph = graphIgraph, graphQgraph = graphQgraph)
  
  
}