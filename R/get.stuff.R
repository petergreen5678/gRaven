get.states<-function(dom,node) dom$states[[node]]

get.children<-function(dom,nodes)
{
.children<-function(dom,node) dom$nodes[which(sapply(dom$parents, function(x) node%in%x))]
if(length(nodes)==1) return(.children(dom,nodes))
else 
{
res<-list()
for(node in nodes) res[[node]]<-.children(dom, node)
res
}
}

get.edges<-function(dom,nodes=dom$nodes) get.children(dom,nodes)



