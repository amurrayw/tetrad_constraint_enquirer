library(igraph)
library(plotrix)
library(Ryacas)

                                        # Takes an adj.matrix of class data.frame (so as to preserve variable names).
get.paths <- function(orig.graph, start.node, end.node, visited=c(), 
                      node.freq=c(), max.freq=3, prev.node=c()){

    list.nodes <- names(orig.graph)

                                        # Note: in adj. matrix, a non-zero symbol in a given row x, column y means
                                        # that row x is a cause of column y.
    location.of.start <- which(list.nodes %in% start.node)
    
    if(is.null(node.freq)){node.freq<-data.frame(matrix(nrow=1, 
                                                        ncol=ncol(orig.graph), 0))}
    
    visited <- c(visited, start.node)

                                        # If we've visted our end node, then we return the path.
    if(end.node %in% visited){
        return(c(visited))
    } 
                                        # subtract 1 from node.freq to make sure that node.freq is equiv.
                                        # to n.times through cycle.
    else if(sum(node.freq-1>max.freq)>0){
                                        # Returns Null if path ended before reaching end.node.
                                        # Otherwise returns path.
        ifelse(test=isFALSE(visited[length(visited)]==end.node),
               yes=return(), no=return(c(visited)))
    }	
    else{
        paths <- c()

        init.adj.nodes <- names(orig.graph)[which(orig.graph[location.of.start,
                                                             ]!="0")]
        init.adj.nodes <- c(init.adj.nodes, 
                            names(orig.graph)[which(
                                orig.graph[,location.of.start]!="0")])
        adj.nodes <- init.adj.nodes[which(! (init.adj.nodes %in% prev.node))]

        node.freq[which(list.nodes %in% adj.nodes)] <- node.freq[
                                                                 which(list.nodes %in% adj.nodes)]+1

        for(node in adj.nodes){
            paths[[node]] <- c(get.paths(orig.graph=orig.graph,
                                         start.node=node,
                                         end.node=end.node, 
                                         visited=visited, 
                                         node.freq=node.freq,
                                         max.freq=max.freq, 
                                         prev.node=c(start.node)))
            
        }
    }
    return(paths)
}

                                        # converts the Tetrad .r.txt representation for a general graph to an 
                                        # adjacency data.frame. General graph cannot have self cycles (i.e., x->x).
read.graph <- function(file, need.plot=FALSE){
    orig.mat <- read.table(file=file)

    orig.mat[orig.mat==1] <-0
    orig.mat[orig.mat==-1] <-1
    diag(orig.mat)<-0
    
    if(need.plot==TRUE){plot(graph.adjacency(as.matrix(orig.mat)))}
    
    final.graph <- create.weighted.graph(graph.adjacency(as.matrix(orig.mat)))

    return(data.frame(final.graph))
}



                                        #changes edge weights from 1 to letters (coefs).
create.weighted.graph <- function(unweighted.graph){
    unweighted.graph<-as.matrix(get.adjacency(unweighted.graph))
    location.edges <- which(unweighted.graph>0)
    
    n.edges <- length(location.edges)

    list.letters <- c(letters, LETTERS)

    if(sum(c(length(letters), length(LETTERS)))<=n.edges){return(NULL)}
    
                                        # Does the same thing as the for-loop, but is more efficent.
    unweighted.graph[location.edges]<-list.letters[1:n.edges]
    
    
                                        #	nextLetter <- 1
                                        #	for(i in location.edges){
                                        #		unweighted.graph[i] <- list.letters[nextLetter]
                                        #		nextLetter <- nextLetter+1
                                        #	}
    return(unweighted.graph)
}


                                        # Converts variable depth list of paths into a depth=1 list of paths.
destroy.list <- function(list.obj){
    path.length <- plumb.depths(list.obj)

    list.obj <- unlist(list.obj)

    temp.list <- list()

    for(i in 1:length(path.length)){
        temp.list[[i]] <- list.obj[1:path.length[i]]
        list.obj<-list.obj[-(1:path.length[i])]
    }

    list.obj <- temp.list

    return(list.obj)
} 


                                        # finds the length of each sublist (path).
plumb.depths <- function(list.obj, depth.level=1){
    if(listDepth(list.obj)>0){
        unlist(lapply(list.obj, function(data){plumb.depths(data, 
                                                            depth.level=depth.level+1)}))
    }
    else{
        return(length(list.obj))
    }

}


                                        # Creates a list of treks with cor. coefs. 
                                        # (Ugly as sin, with none of the benefits. Needs commented)
get.trek <- function(adj.mat, path.list){

    lapply(path.list, function(vec){

	trek.vec <- c()
	for(i in 1:(length(vec)-1)){
                                        # The location of edge coef. changes depending on whether it is for a 
                                        # cause or an effect. This command checks one of the positions. If the
                                        # element is 0 (i.e, the wrong one), the command gets the 
                                        # coef from the other.
            if(sum((adj.mat[[which(names(adj.mat)%in%vec[i]), 
                             which(names(adj.mat)%in%vec[i+1])]])==0)==0){
                trek.vec <- paste(trek.vec, adj.mat[[
                                                     which(names(adj.mat)%in%vec[i]), 
                                                     which(names(adj.mat)%in%vec[i+1])
                                                     ]],
                                  sep="*")
            }
            else{
                trek.vec <- paste(trek.vec, adj.mat[[
                                                     which(names(adj.mat)%in%vec[i+1]), 
                                                     which(names(adj.mat)%in%vec[i])
                                                     ]],
                                  sep="*")
            }
            
	}
	return(paste(unlist(lapply(trek.vec, function(trek){
            return(paste(substring(trek, 2, nchar(trek)), "+"))
        }))))
    })
}

                                        # Combines the seperate treks together into a string.
combine.trek <- function(trek){
    trek <- unlist(trek)
    trek <- paste(trek, sep="", collapse=" ") 

    return(substring(trek, 1, nchar(trek)-1))
}

                                        # Swaps the sign on a boolean. Is used as a more explicit version of "!".
isFALSE <- function(truth.val){
    if(truth.val==FALSE){return(TRUE)}
    else{return(FALSE)}
}

                                        # Checks whether, given two inputs and two outputs, the difference is zero.
check.difference <- function(given.graph=temp, input.1, input.2, output.1, output.2, 
                             max.freq=1){
    
    trek.1 <- combine.trek(get.trek(given.graph, 
                                    destroy.list(get.paths(
                                        orig.graph=given.graph, 
                                        input.1, 
                                        output.1, 
                                        max.freq=max.freq))))
    trek.2 <- combine.trek(get.trek(given.graph, 
                                    destroy.list(get.paths(
                                        orig.graph=given.graph, 
                                        input.2, 
                                        output.2,
                                        max.freq=max.freq))))
    trek.3 <- combine.trek(get.trek(given.graph, 
                                    destroy.list(get.paths(
                                        orig.graph=given.graph,
                                        input.2, output.1, 
                                        max.freq=max.freq))))
    trek.4 <- combine.trek(get.trek(given.graph, 
                                    destroy.list(get.paths(
                                        orig.graph=given.graph, 
                                        input.1, 
                                        output.2, 
                                        max.freq=max.freq))))
    
    constraint <- (paste( "(", trek.1, ")", "*", 
                         "(", trek.2, ")", "-(", 
                         "(", trek.3, ")", "*", 
                         "(", trek.4, ")", ")", 
                         sep="", collapse=" "))
    
    evaled.constraint <- yacas(Simplify(constraint), retclass="character")

                                        # If the constraint holds, return true. Otherwise,return the constraint.
    if(isFALSE(is.null(evaled.constraint$YacasForm)) &&
       unlist(strsplit(evaled.constraint$YacasForm, split=";", fixed=""))==0){
        return(TRUE)
    }
    else{return(constraint)}
}



