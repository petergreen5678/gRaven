get.states<-function(dom,node) dom$states[[node]]

get.children<-function(dom,node) dom$nodes[which(sapply(dom$parents,function(x) node%in%x))]

get.edges<-function(dom) structure(lapply(dom$nodes,function(x) get.children(dom,x)),names=dom$nodes)




