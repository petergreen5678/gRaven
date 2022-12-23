# edits to allow logical states

get.states<-function (dom, node=dom$nodes)
{
# assuming logicals coded as 0 and 1 in the order specified in add.node (differs from Hugin?)
if(length(node)==1)
{
x<-dom$states[[node]]
y<-attr(x,'logical')
if(is.null(y)) x else y[x+1]
}
else
sapply(node,function(x) get.states(dom,x))
}

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



