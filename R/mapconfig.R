.getfinding <- function(dom,node)
{
  i <- match(node, dom$net$evidence$nodes)
  ## No evidence corresponds to constant evidence of 1
  if(is.na(i)) return(rep(1,length(dom$states[[node]])))
  as.vector(dom$net$evidence$evi_weight[[i]])
}

map.configurations <-function (dom, nodes, pmin) 
{
  if (pmin <= 0 || pmin > 1) 
    stop("pmin is not between 0 and 1")
  if(!dom$net$isPropagated) propagate(dom)

  n <- 1000              # Batch size for simulations
  conf <- NULL           # data.frame of seen configurations
  p <- numeric(0L)       # Probability vector for seen configurations
  totalp <- 0            # Total probability of seen configurations
  i <- 0                 # Row index of last computed probability
  size <- length(nodes)  # Number of nodes for which we find configurations
  stopifnot("Cannot find the most probable configurations of zero nodes" = size > 0)  
  
  ## States and pre-existing findings on target nodes
  states <- list()
  findings <- list()
  for(j in seq_along(nodes)) 
  {
    states[[j]] <- dom$states[[nodes[j]]]
    findings[[j]] <- .getfinding(dom,nodes[j])
  }
  
  ## Second domain for computations (takes extra memory!)
  ## The state of the original network is left unaltered,
  ## so that simulations from it are from the correct distribution.
  dom2 <- clone.domain(dom)
  
  ## Normalisation constant *before* entering configuration of interest
  ## (but after any initially propagated evidence)
  nc1 <- get.normalization.constant(dom2) 
  
  while (totalp < 1-pmin){
  
    ## Simulate batch of n configurations, keep only relevant nodes
    sg <- simulate.grain(dom$net, n)
    sg <- subset(sg, select = nodes) 
    
    ## Change state values from factors to character
    for(node in nodes) {
      sg[,node]<-dom$states[[node]][as.integer(sg[,node])]
    }
    
    ## merge onto already found configurations, if any
    conf <- rbind(conf, sg)
    conf <- conf[!duplicated(conf), , drop = FALSE] 
    
    ## Compute the exact probability of each newly found configuration, if any
    if (nrow(conf) > i){
      ## Loop through new configurations
      for (r in (i+1):nrow(conf)){
        
        ## Enter a new configuration
        lapply(1:size, function(j)set.finding(dom2, nodes[j], as.integer(conf[r,j] == states[[j]])*findings[[j]]))

        ## Get configuration probability as the ratio of normalising constants 
        ## after vs before entering configuration
        p[r] <- get.normalization.constant(dom2)/nc1
      }

      ## Record the last row with computed probability, 
      ## so it is not recomputed repeatedly
      i <- nrow(conf)
      totalp <- sum(p)
    }
  }
  ## All configurations with probability at least pmin
  out <- as.data.frame(cbind(conf, Prob = p), row.names = NULL)
  out <- out[out$Prob > pmin,]
  out <- out[order(out$Prob, decreasing = TRUE),]
  rownames(out) <- NULL
  out
} 