## **The correctness of this implementation is based on an assumption that no evidence is entered already on the nodes of interest.
map.configurations<-function (dom, nodes, pmin) 
{
  if(!dom$net$isPropagated) propagate(dom)

  ## Batch size for simulations
  n <- 1000
  conf <- NULL
  totalp <- 0
  p <- numeric(0L) # Probabilities
  i <- 0 # Row index of last computed probability
  size <- length(nodes) 
  
  ## Second domain for computations (takes extra memory!)
  ## The state of the original network is left unaltered,
  ## so that simulations from it are from the correct distribution.
  dom2 <- clone.domain(dom)
  
  ## Normalisation constant *before* entering configuration of interest
  ## (but after any initially propagated evidence)
  # **Consider retracting first evidence on "nodes" for robustness?
  nc1 <- get.normalization.constant(dom2)
  
  while (totalp < 1-pmin){
  
    ## Simulate n configurations, keep only relevant nodes
    sg <- simulate.grain(dom$net, n)
    sg <- subset(sg, select = nodes)
    
    ## merge onto already found configurations, if any
    conf <- rbind(conf, sg)
    conf <- conf[!duplicated(conf),] # Note: comparing also existing configs
    
    ## Compute the exact probability of each newly found configuration, if any
    if (nrow(conf) > i){
      for (r in (i+1):nrow(conf)){
    
        # Propagate the new configuration and get its probability.
        # No need to retract the old configuration, as the new overwrites the old evidence.
        lapply(1:size, function(j)set.finding(dom2, nodes[j], conf[r,j]))
        propagate(dom2)
        ## ratio of normalising constants before and *after* entering configuration
        p[r] <- get.normalization.constant(dom2)/nc1
      }
      ## Record the last row with computed probability, 
      ## so it is not recomputed repeatedly
      i <- nrow(conf)
      totalp <- sum(p)
    }
  }
  for(j in nodes) conf[,j]<-dom$states[[j]][as.integer(conf[,j])]

  ## All configurations with probability at least pmin
  out <- as.data.frame(cbind(conf, Prob = p), row.names = NULL)
  out <- out[out$Prob > pmin,]
  out <- out[order(out$Prob, decreasing = TRUE),]
  rownames(out) <- NULL
  out
}  
    
  