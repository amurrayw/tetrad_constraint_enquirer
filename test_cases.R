
############ Test Cases ###############

#require(igraph)


# Testing three-node length single path case
temp<-matrix(nrow=5, ncol=5, "0")
temp[1,] <-c("0", "0", "1", "1", "0")
temp[3,] <-c("0", "0", "0", "0", "1")
temp<-data.frame(temp)
names(temp)<-1:ncol(temp)

get.paths(orig.graph=temp, 1, 5)


# Testing three-node length two path case
temp<-matrix(nrow=5, ncol=5, "0")
temp[1,] <-c(0,0, 1, 1, 0)
temp[3,] <-c(0,0, 0, 0, 1)
temp[4,] <-c(0,0, 0, 0, 1)
temp<-data.frame(temp)
names(temp)<-1:ncol(temp)

get.paths(orig.graph=temp, 1, 5)



# Testing four-node length three path case.
temp<-matrix(nrow=5, ncol=5, "0")
temp[1,] <-c(0,0, 1, 1, 0)
temp[3,] <-c(0,0, 0, 0, 1)
temp[4,] <-c(0,1, 0, 0, 1)
temp[2,] <-c(0,0, 0, 0, 1)

temp<-data.frame(temp)
names(temp)<-1:ncol(temp)

get.paths(orig.graph=temp, 1, 5)



# Testing four-node length three path cyclic case. 
temp<-matrix(nrow=5, ncol=5, 0)
temp[1,] <-c(0,0, 1, 1, 0)
temp[3,] <-c(0,0, 0, 0, 1)
temp[4,] <-c(0,1, 0, 0, 1)
temp[2,] <-c(1,0, 0, 0, 1)

temp<-data.frame(temp)
names(temp)<-1:ncol(temp)

get.paths(orig.graph=temp, 1, 5, max.freq=3)


temp<-matrix(nrow=5, ncol=5, "0")
temp[1,] <-c("0","0", "a", "b", "0")
temp[3,] <-c("0","0", "0", "0", "c")
temp[4,] <-c("0","d", "0", "0", "e")
temp[2,] <-c("f","0", "0", "0", "g")

temp<-data.frame(temp)
#row.names(temp) <- c(1,2,3,4,5)
#names(temp) <- c(1,2,3,4,5)

get.paths(orig.graph=temp, "X1", "X5", max.freq=3)



#Extract all of the different paths into a list of depth 2.

asdf <- destroy.list(get.paths(orig.graph=temp, "X1", "X5", max.freq=3))


#Simplify the trek expression

Simplify(combine.trek(get.trek(temp, asdf)))


 


Simplify(combine.trek(get.trek(temp, destroy.list(get.paths(orig.graph=temp, "X1", "X5", max.freq=5)))))

temp<-read.graph(file="~/Dropbox/school/research/clark_paper/graph1.r.txt")
temp <- data.frame(temp)

#get.paths(orig.graph=temp, "X9", "X6", max.freq=2)

one.trek.1<-(combine.trek(get.trek(temp, destroy.list(get.paths(orig.graph=temp, "X9", "X6", max.freq=2)))))
one.trek.2<-(combine.trek(get.trek(temp, destroy.list(get.paths(orig.graph=temp, "X11", "X6", max.freq=2)))))

other.trek.1<-(combine.trek(get.trek(temp, destroy.list(get.paths(orig.graph=temp, "X10", "X3", max.freq=2)))))
other.trek.2<-(combine.trek(get.trek(temp, destroy.list(get.paths(orig.graph=temp, "X12", "X3", max.freq=2)))))

c(one.trek, other.trek)


# TODO: Need to figure out how to do something like this.
Simplify(paste(c(one.trek.1, one.trek.2), "-", c(other.trek.1, other.trek.2), sep="", collapse="")) 

# TODO: Need to write an easy to use interface for trek software.



#Simplify(paste(combine.trek(get.trek(temp, destroy.list(get.paths(orig.graph=temp, "X9", "X5", max.freq=3)))), "*", combine.trek(get.trek(temp, destroy.list(get.paths(orig.graph=temp, "X9", "X4", max.freq=3))))), sep="", collapse="")

# David Heise "Causal models"
# Look up Mason's rules (put in terms of gain of an amplifier). Electrical enginering. cyclical treks







