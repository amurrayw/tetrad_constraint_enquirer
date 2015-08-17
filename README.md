This program reports on whether an inputed graph contains a given tetrad constraint.Installation:

Need to install: 

Base:
1. R - get from: http://lib.stat.cmu.edu/R/CRAN/
2. yacas - get from: http://yacas.sourceforge.net/homepage.html (for Windows)
					 http://www.mathdrake.com/yacas/index.html (for Mac)

R packages:
1. igraph
2. plotrix
3. ryacas

This can be done by running the following command in R:

install.packages(c("igraph", "plotrix", "ryacas"))

To install as fast as possible, select the PA 1 repository when prompted.

Running:

1. Start R.

2. Set your working directory to wherever the folder containing get_paths.R is. This can be done by running:

setwd("location.of.directory.or.folder.where.files.are")

3. Run the following command in R:

source("get_paths.R")

4. Read in the .r.txt graph file you outputed from Tetrad using the following commands:

name.of.graph<-read.graph(file="name.of.graph.r.txt", need.plot=TRUE) # need.plot lets you output a plot so that you can check that the graph has read in correctly.
name.of.graph <- data.frame(name.of.graph) # Converts the adjacency matrix to a form the constraint test requires. 

5. Run the constraint test for whatever variables you want. For example:

check.difference(graph1, "X12", "X9", "X2", "X4", max.freq=1)

6. If you prefer a reduced output (when the constraint fails to hold), you can run: 

Simplify(check.difference(graph1, "X12", "X9", "X2", "X4", max.freq=1))












