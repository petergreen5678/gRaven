summary.gRv<-function (object, domain = TRUE, nodes = FALSE, jt = FALSE, print.cliques = FALSE, 
    ...) 
{
    if(nodes|print.cliques) warning('not all options yet implemented')
    if (is.logical(nodes)) {
        if (nodes) 
            nodes <- get.nodes(object)
        else nodes <- character(0)
    }
    compiled <- object$net$isCompiled; if(is.null(compiled)) compiled<-FALSE
    domain.summary <- NULL
    if (domain) {
        compressed <- FALSE
        if (compiled) {
            propagated <- object$net$isPropagated; if(is.null(propagated)) propagated<-FALSE
            evidence.to.propagate<-!is.null(object$net$cache)
        }
        else {
            propagated <- NULL
            evidence.to.propagate<-NULL
        }
        domain.summary <- list(compiled = compiled, compressed = compressed, propagated = propagated,
		evidence.to.propagate=evidence.to.propagate)
    }
    jt.summary <- NULL
    if(jt&&compiled) 
	{
	jt.summary<-list(cliques=object$net$rip$cliques,parents=object$net$rip$parents)
	}
res<-list(domain=domain.summary,jt=jt.summary)
structure(res,class="summary.gRv")
}

print.summary.gRv<-function(x, ...)
{
cat('gRaven Domain:\n')
cat(ifelse(x$domain$compiled,'   is compiled\n','   is not compiled\n'))
cat(ifelse(x$domain$compressed,'   is compressed\n','   is not compressed\n'))
cat(ifelse(x$domain$propagated,'   is propagated\n','   is not propagated\n'))
cat(ifelse(x$domain$evidence.to.propagate,'   evidence','   no evidence'),'has been entered since the last propagation\n')
if(!is.null(x$jt)) 
{
cat('Junction Tree:\n')
cliqs<-x$jt$cliques
for(i in seq_along(cliqs)) cat('   clique',paste0(i,':'),cliqs[[i]],'\n')
p<-x$jt$parents
up<-p[(p!=0)&(!duplicated(p))]
#for(u in up) cat(u,'->', which(p==u),' /  ')
#cat('\n')
buffer<-''
for(u in up) 
{
buffer<-paste(buffer,paste(u,'->', paste(which(p==u),collapse=',')),sep=' / ')
if(nchar(buffer)>50) {cat('   ',buffer,'\n'); buffer<-''}
}
cat('   ',buffer,'\n')
}
}
